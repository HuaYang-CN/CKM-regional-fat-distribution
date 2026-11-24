"""Compare independently computed aggregate NHANES outputs with manuscript precision."""
import argparse
import csv
from pathlib import Path

def read_tsv(path):
    with path.open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    parser.add_argument("report", type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    key_fields = {"flow": "step", "stage_counts": "stage", "models": "model", "diagnostics": "metric"}
    datasets = {}
    for table, key in key_fields.items():
        rows = read_tsv(args.results / (table + ".tsv"))
        datasets[table] = {row[key]: row for row in rows}
        if len(datasets[table]) != len(rows):
            raise ValueError("Duplicate keys in " + table)
    checks = read_tsv(root / "manifests/public_reference.tsv")
    failures = 0
    for check in checks:
        data = datasets[check["table"]][check["key"]]
        actual = data[check["field"]]
        target = check["target"]
        if target.startswith("<"):
            passed = float(actual) < float(target[1:])
        else:
            decimals = len(target.partition(".")[2])
            passed = f"{float(actual):.{decimals}f}" == target
        check["actual"] = actual
        check["status"] = "MATCH" if passed else "DIFFERENT"
        failures += not passed
    with args.report.open("x", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(checks[0]), delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(checks)
    print(f"{len(checks)-failures}/{len(checks)} targets matched")
    return 1 if failures else 0

if __name__ == "__main__":
    raise SystemExit(main())
