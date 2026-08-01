---
description: Release the R package — bump version, verify, merge develop into main, tag, push, and create a GitHub release
allowed-tools: Bash(git *), Bash(gh *), Bash(Rscript *)
---

# R Package Release Skill

Bump the package version, run checks, merge `develop` into `main`, tag, push, create a GitHub release, and bump `develop` to a `.9000` dev version.

## Usage

```text
/rpkg-release <version-or-bump> [--skip-checks]
```

The argument can be an explicit version number or a semantic bump descriptor:

- `/rpkg-release 0.6.0` — release as version 0.6.0
- `/rpkg-release major` — bump the major version (e.g., 0.5.1 -> 1.0.0)
- `/rpkg-release minor` or `/rpkg-release minor bump` — bump the minor version (e.g., 0.5.1 -> 0.6.0)
- `/rpkg-release patch` — bump the patch version (e.g., 0.5.1 -> 0.5.2)
- `/rpkg-release as a major change` — natural language works too; extract the bump level

Any phrasing that contains "major", "minor", or "patch" should be interpreted as that bump level. If ambiguous, ask the user.

To skip R CMD check, the user can say `--skip-checks`, `skip checks`, `no checks`, `already checked`, or any natural language indicating checks should be skipped.

## Prerequisites

- Must be on the `develop` branch with a clean working tree (no uncommitted changes)
- All changes intended for the release must already be committed on `develop`

## Instructions for Claude

### Step 1: Validate state

1. Confirm on `develop` branch: `git branch --show-current`
2. Confirm clean working tree: `git status --porcelain` must be empty
3. If not clean or not on `develop`, stop and tell the user

### Step 2: Determine version

1. Read the current `Version` from `DESCRIPTION`. If it has a `.9000` (or higher fourth-digit) dev suffix (e.g., `0.7.1.9000`), use the base version (`0.7.1`) as the comparison point for bumps.
2. If the user provided an explicit version (e.g., `0.6.0`), use it
3. If the user provided a bump descriptor, compute the new version using semver from the base (non-dev) version:
   - **major**: increment major, reset minor and patch to 0 (e.g., 0.5.1 -> 1.0.0)
   - **minor**: increment minor, reset patch to 0 (e.g., 0.5.1 -> 0.6.0)
   - **patch**: increment patch (e.g., 0.5.1 -> 0.5.2)
4. Confirm the computed version with the user before proceeding

### Step 3: Bump version

1. Update `Version` and `Date` in `DESCRIPTION`
   - Version: the determined version
   - Date: use today's date (YYYY-MM-DD)
2. Regenerate docs: `Rscript -e 'devtools::document()'`
3. Rebuild README: `Rscript -e 'devtools::build_readme(quiet = TRUE)'`
4. Commit the version bump using the `/commit` skill conventions:

   ```text
   docs: bump version to <version>
   ```

### Step 4: Run R CMD check (unless `--skip-checks`)

```bash
Rscript -e 'devtools::check()'
```

- If check passes with 0 errors and 0 warnings, proceed
- If there are errors or warnings, stop and report them to the user
- Notes are acceptable — mention them but proceed

### Step 5: Merge into main

```bash
git checkout main
git pull origin main
git merge --no-ff develop -m "Merge branch 'develop' for v<version> release"
```

### Step 6: Tag the release

```bash
git tag -a v<version> -m "v<version>"
```

### Step 7: Push

```bash
git push origin main develop --tags
```

### Step 8: Create GitHub release

Derive release notes from the commits included in this release:

```bash
git log --oneline <previous-tag>..v<version>
```

If no previous tag exists, use all commits on main.

Create the release:

```bash
gh release create v<version> --title "v<version>" --notes "$(cat <<'EOF'
<release notes>
EOF
)"
```

Release notes format:

```markdown
## What's changed

- [grouped summary of changes, derived from commit messages]

**Full changelog**: <previous-tag>...v<version>
```

### Step 9: Return to develop

```bash
git checkout develop
```

### Step 10: Bump develop to dev version

After a release, `develop` should be ahead of the released version with a `.9000` dev suffix so anyone installing from `develop` can tell they're on in-development code (and not the released version).

1. Set `Version` in `DESCRIPTION` to `<released-version>.9000` (keep `Date` as the release date)
2. Commit:

   ```text
   chore: bump develop to <released-version>.9000
   ```

3. Push develop:

   ```bash
   git push origin develop
   ```

Report the release URL to the user.

## Important

- Do NOT add a `Co-Authored-By` line or reference AI anywhere.
- Do NOT skip R CMD check unless the user explicitly passes `--skip-checks`.
- If anything fails (check errors, merge conflicts, push rejection), stop and report — do not force through.
