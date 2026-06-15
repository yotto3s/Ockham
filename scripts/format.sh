#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DRY_RUN=0
TARGET_DIR="$PROJECT_ROOT"

usage() {
    cat <<EOF
Usage: $(basename "$0") [--dry-run] [DIRECTORY]

Run clang-format on all C source and header files recursively.

Options:
  --dry-run     Check formatting without modifying files (exits non-zero if
                any file needs reformatting)
  -h, --help    Show this message

Arguments:
  DIRECTORY     Directory to search (default: project root)
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        -h | --help)
            usage
            ;;
        -*)
            echo "error: unknown option: $1" >&2
            echo "Run '$(basename "$0") --help' for usage." >&2
            exit 1
            ;;
        *)
            TARGET_DIR="$1"
            shift
            ;;
    esac
done

if ! command -v clang-format &>/dev/null; then
    echo "error: clang-format not found in PATH" >&2
    exit 1
fi

mapfile -d '' FILES < <(find "$TARGET_DIR" \
    -type d \( -name build -o -name .git \) -prune \
    -o -type f \( -name "*.c" -o -name "*.h" \) -print0 \
    | sort -z)

if [[ ${#FILES[@]} -eq 0 ]]; then
    echo "No C files found in $TARGET_DIR"
    exit 0
fi

if [[ $DRY_RUN -eq 1 ]]; then
    echo "Checking formatting in: $TARGET_DIR"
    clang-format --dry-run --Werror "${FILES[@]}"
    echo "All files are properly formatted."
else
    echo "Formatting files in: $TARGET_DIR"
    clang-format -i "${FILES[@]}"
    echo "Done. ${#FILES[@]} file(s) processed."
fi
