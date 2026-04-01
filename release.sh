#!/usr/bin/env bash
set -euo pipefail

VERSION="${1:?Usage: ./release.sh <version> (e.g. 0.2.0)}"
TAG="v${VERSION}"
MACOS_BINARY="korb-aarch64-macos"

if git ls-remote --tags origin | grep -q "refs/tags/${TAG}$"; then
  echo "Error: tag ${TAG} already exists on remote"
  exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
  echo "Error: working tree is dirty. Commit or stash changes first."
  exit 1
fi

echo "=== Updating version in korb.cabal ==="
sed -i '' "s/^version:.*$/version:            ${VERSION}.0/" korb.cabal

echo "=== Building macOS binary ==="
cabal build
BUILT=$(cabal list-bin korb)
cp "${BUILT}" "${MACOS_BINARY}"
strip "${MACOS_BINARY}"
echo "macOS binary: ${MACOS_BINARY} ($(du -h "${MACOS_BINARY}" | cut -f1))"

echo "=== Committing and pushing ==="
git add korb.cabal
git commit -m "Release ${TAG}"
git push origin main

echo "=== Creating GitHub release (creates tag, uploads macOS binary) ==="
echo "=== Linux binary will be built and uploaded by GitHub Actions ==="
gh release create "${TAG}" "${MACOS_BINARY}" \
  --title "${TAG}" \
  --target main \
  --generate-notes

rm "${MACOS_BINARY}"
echo "=== Done: ${TAG} ==="
echo "Linux binary will appear on the release once the GitHub Action completes."
