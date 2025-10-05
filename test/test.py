import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import List

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "InCollege.cob"
BINARY = ROOT / "incollege"
DATA_DIR = ROOT / "data"
SCENARIOS_DIR = ROOT / "test" / "inputs"
RESULTS_DIR = ROOT / "test" / "outputs"


def compile_program() -> None:
    needs_compile = (not BINARY.exists()) or (BINARY.stat().st_mtime < SOURCE.stat().st_mtime)
    if needs_compile:
        subprocess.run(
            ["cobc", "-x", "-o", str(BINARY), str(SOURCE)],
            check=True,
            cwd=ROOT,
        )


def read_lines(path: Path) -> List[str]:
    if not path.exists():
        return []
    return path.read_text(encoding="ascii").splitlines()


def read_non_empty_lines(path: Path) -> List[str]:
    return [line for line in read_lines(path) if line.strip()]


@dataclass
class Scenario:
    name: str
    description: str
    runs: List[List[str]]
    initial_connections: List[str]
    expected_connections: List[str]
    must_contain: List[str]
    must_exclude: List[str]


def load_scenarios() -> List[Scenario]:
    scenarios: List[Scenario] = []
    if not SCENARIOS_DIR.exists():
        return scenarios

    # Read input files directly from the inputs directory
    for input_file in sorted(SCENARIOS_DIR.glob("*_input.txt")):
        # Extract scenario name from filename (e.g., 01_accept_multiple_requests_input.txt)
        name = input_file.stem.replace("_input", "")

        raw_inputs = read_lines(input_file)
        runs: List[List[str]] = []
        current: List[str] = []
        for line in raw_inputs:
            if line.strip() == "---":
                if current:
                    runs.append(current)
                    current = []
            else:
                current.append(line)
        if current:
            runs.append(current)

        if not runs:
            raise ValueError(f"Scenario '{name}' has no inputs")

        # Load expected connections file
        expected_file = SCENARIOS_DIR / f"{name}_expected.txt"
        expected_connections = read_non_empty_lines(expected_file)

        scenarios.append(
            Scenario(
                name=name,
                description=name,
                runs=runs,
                initial_connections=[],
                expected_connections=expected_connections,
                must_contain=[],
                must_exclude=[],
            )
        )
    return scenarios


@dataclass
class ScenarioResult:
    scenario: Scenario
    passed: bool
    output_lines: List[str]
    issues: List[str]


def run_program(temp_dir: Path) -> List[str]:
    result = subprocess.run(
        [str(BINARY)],
        cwd=temp_dir,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"Program exited with status {result.returncode}: {result.stderr.strip()}"
        )
    output_path = temp_dir / "InCollege-Output.txt"
    if not output_path.exists():
        return []
    return output_path.read_text(encoding="ascii").splitlines()


def execute_scenario(scenario: Scenario) -> ScenarioResult:
    with tempfile.TemporaryDirectory(prefix=f"incollege-{scenario.name}-") as tmp:
        temp_dir = Path(tmp)
        for fixture in ("Accounts.dat", "Profiles.dat"):
            shutil.copy(DATA_DIR / fixture, temp_dir / fixture)
        connections_path = temp_dir / "Connections.dat"
        connections_text = "\n".join(scenario.initial_connections)
        if connections_text:
            connections_text += "\n"
        connections_path.write_text(connections_text, encoding="ascii")

        collected_output: List[str] = []
        issues: List[str] = []

        for run_inputs in scenario.runs:
            input_text = "\n".join(run_inputs)
            if input_text:
                input_text += "\n"
            (temp_dir / "InCollege-Input.txt").write_text(input_text, encoding="ascii")
            (temp_dir / "InCollege-Output.txt").unlink(missing_ok=True)

            collected_output.extend(run_program(temp_dir))

        for expected in scenario.must_contain:
            if not any(expected in line for line in collected_output):
                issues.append(f"Missing expected text: '{expected}'")

        for forbidden in scenario.must_exclude:
            if any(forbidden in line for line in collected_output):
                issues.append(f"Unexpected text present: '{forbidden}'")

        final_connections = [
            line for line in connections_path.read_text(encoding="ascii").splitlines() if line
        ]
        if final_connections != scenario.expected_connections:
            issues.append(
                f"Expected connections {scenario.expected_connections}, got {final_connections}"
            )

        passed = not issues
        return ScenarioResult(
            scenario=scenario,
            passed=passed,
            output_lines=collected_output,
            issues=issues,
        )


def write_results(results: List[ScenarioResult]) -> None:
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    summary_lines: List[str] = []
    for index, result in enumerate(results, start=1):
        scenario = result.scenario
        base_name = f"{index:02d}_{scenario.name}"
        output_path = RESULTS_DIR / f"{base_name}_output.txt"
        output_path.write_text(
            "\n".join(result.output_lines) + ("\n" if result.output_lines else ""),
            encoding="ascii",
        )

        summary_lines.append(
            f"Test {index}: \"{scenario.description}\" - {'PASS' if result.passed else 'FAILED'}"
        )
        if result.issues:
            for issue in result.issues:
                summary_lines.append(f"    - {issue}")

    summary_path = RESULTS_DIR / "summary.txt"
    summary_path.write_text("\n".join(summary_lines) + "\n", encoding="ascii")


def print_console_summary(results: List[ScenarioResult]) -> None:
    for result in results:
        scenario = result.scenario
        status = "PASSED" if result.passed else "FAILED"
        print(f"{scenario.name}_input.txt - {status}")


def main() -> int:
    try:
        compile_program()
    except subprocess.CalledProcessError as exc:
        print(f"Compilation failed: {exc}", file=sys.stderr)
        return 1

    try:
        scenarios = load_scenarios()
    except ValueError as exc:
        print(f"Error loading scenarios: {exc}", file=sys.stderr)
        return 1

    if not scenarios:
        print("No scenarios found in test/inputs", file=sys.stderr)
        return 1

    results: List[ScenarioResult] = []
    for scenario in scenarios:
        try:
            result = execute_scenario(scenario)
        except RuntimeError as exc:
            result = ScenarioResult(
                scenario=scenario,
                passed=False,
                output_lines=[str(exc)],
                issues=[str(exc)],
            )
        results.append(result)

    print_console_summary(results)
    write_results(results)

    return 0 if all(r.passed for r in results) else 1


if __name__ == "__main__":
    sys.exit(main())
