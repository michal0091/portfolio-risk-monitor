#!/usr/bin/env bash
# Update the GitHub secrets with the personal configuration (git-ignored files).
# Usage: ./update_secrets.sh              both files
#        ./update_secrets.sh portfolio    only cfg/portfolio.yaml -> PORTFOLIO_YAML_B64
#        ./update_secrets.sh positions    only cfg/positions.yaml -> POSITIONS_YAML_B64
# Each file is parsed first, so a broken YAML never reaches the secret.
set -euo pipefail
cd "$(dirname "$0")"

targets=("${@:-portfolio positions}")
for t in ${targets[*]}; do
  case "$t" in
    portfolio|positions) ;;
    *) echo "Unknown target '$t' (use portfolio or positions)" >&2; exit 1 ;;
  esac
  file="cfg/$t.yaml"
  secret="$(echo "$t" | tr '[:lower:]' '[:upper:]')_YAML_B64"
  if [ ! -f "$file" ]; then echo "Skip $t: $file not found"; continue; fi
  Rscript -e "invisible(yaml::read_yaml('$file', fileEncoding = 'UTF-8'))" \
    || { echo "Invalid YAML in $file: secret not updated" >&2; exit 1; }
  gh secret set "$secret" --body "$(base64 -w0 "$file")"
done
gh secret list
