# Legacy Compatibility

All legacy logic in `src/Hald/Legacy.hs` plus boot-script fallback code
(annotated `# LEGACY`).

## Legacy ald Root (`/.ald/`)
- Detection: `Hald.Legacy.detectLegacyPaths` at startup
- Fallback: `resolveRootDir`, `resolveLockfile`, `findDeploymentIds`
- Condition: `/.ald/` exists & is not a symlink to `haldPath`

## Flat Deployment Paths (`<haldPath>/<id>`)
- Scanned alongside `<haldPath>/trees/<id>`
- Used in: `resolveRootDir`, `resolveLockfile`, `findDeploymentIds`
- Condition: flat `<id>` dir exists at haldPath root

## Legacy Marker Files (`.ald_dep`, `.ald_assetmap`)
- `.ald_dep` fallback in: `readDepLockfile`

## Kernel Cmdline Fallback (`ald.boot`)
- Checked after `hald.boot` in: `hald-boot.sh`, `init-bottom-script`
