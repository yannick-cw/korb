#!/usr/bin/env bash
set -euo pipefail

VERSION="${1:?Usage: ./release.sh <version> (e.g. 0.2.0)}"
TAG="v${VERSION}"
BINARY_NAME="korb-aarch64-macos"

if git tag -l "${TAG}" | grep -q "${TAG}"; then
  echo "Error: tag ${TAG} already exists"
  exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
  echo "Error: working tree is dirty. Commit or stash changes first."
  exit 1
fi

echo "=== Building korb ${VERSION} ==="
cabal build

echo "=== Copying and stripping binary ==="
BUILT=$(cabal list-bin korb)
cp "${BUILT}" "${BINARY_NAME}"
strip "${BINARY_NAME}"
STRIPPED_SIZE=$(du -h "${BINARY_NAME}" | cut -f1)
echo "Binary: ${BINARY_NAME} (${STRIPPED_SIZE})"

echo "=== Updating version in korb.cabal ==="
sed -i '' "s/^version:.*$/version:            ${VERSION}.0/" korb.cabal
git add korb.cabal
git commit -m "Release ${TAG}"
git tag "${TAG}"

echo "=== Pushing to GitHub ==="
git push origin main
git push origin "${TAG}"

echo "=== Creating GitHub release ==="
gh release create "${TAG}" "${BINARY_NAME}" \
  --title "${TAG}" \
  --generate-notes

rm "${BINARY_NAME}"
echo "=== Done: ${TAG} ==="
