# Contributing to GLaDOS

Thank you for your interest in contributing to GLaDOS! This document provides everything you need to know to get started with contributing to the project.

## Prerequisites

Before you start contributing, ensure you have met all the prerequisites listed in the [README.md](./README.md#prerequisites). You'll need:

- **Stack** - Haskell project and dependency manager
- **GNU Make** - Build automation tool
- **Git** - For cloning the repository

For detailed setup instructions, refer to the [README.md](./README.md).

## Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/rune-prog-lang/glados
cd glados
```

### 2. Setup Git Hooks (Optional but Recommended)

To ensure code quality and prevent commits with bad naming conventions, install the pre-push hook:

```bash
./scripts/install_hooks.sh
```

This script will:
- Install a pre-push hook that validates commit message conventions before pushing
- Prevent accidental pushes with incorrectly formatted commit messages
- Run automatically each time you attempt to push

## Contributing Workflow

### Step 1: Create a Feature Branch

You have two options for creating a branch:

**Option A: Create a branch from a GitHub Issue (Recommended)**

1. Go to the [GitHub Issues](https://github.com/rune-prog-lang/glados/issues)
2. Open the issue you want to work on
3. Click the "Create a branch" button (usually in the sidebar)
4. GitHub will create a branch with a name like `<issue-number>-issue-title`
5. Clone or fetch the branch locally:
   ```bash
   git fetch origin
   git checkout <issue-number>-issue-title
   ```

This approach:
- Automatically links your work to the issue
- Creates a clear relationship between issue and PR
- Makes it easy to track what you're working on

**Option B: Create a branch locally**

Create a new branch for your work:

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/your-fix-name
```

Branch naming suggestions:
- `feature/` - For new features
- `fix/` - For bug fixes
- `docs/` - For documentation updates
- `refactor/` - For code refactoring
- `<issue-number>-` - Include issue number if applicable (e.g., `fix/123-parser-bug`)

### Step 2: Make Your Changes

Implement your fix, feature, or improvement in your branch.

### Step 3: Commit Your Changes

Use the provided commit script to ensure your commit follows the naming convention:

```bash
./scripts/normed_commit.sh <git add args>
```

Examples:
```bash
./scripts/normed_commit.sh -A          # Add all changes
./scripts/normed_commit.sh .           # Add current directory
./scripts/normed_commit.sh file1 file2 # Add specific files
```

The script will guide you through the commit process and offer these commit types:

**PATCH** (fixes and improvements):
- `fix` - Bug fix
- `perf` - Performance improvement
- `refactor` - Code refactoring without functional change

**MINOR** (new features):
- `feat` - New feature

**MAJOR** (breaking changes):
- `BREAKING` - Major incompatible change (should include BREAKING CHANGE in the message)

**Others** (no semantic versioning impact):
- `docs` - Documentation changes
- `style` - Code style / formatting changes
- `chore` - Maintenance tasks
- `test` - Add or fix tests
- `build` - Build system changes
- `ci` - CI/CD changes

The script will ask for:
1. **Commit type** (from the list above)
2. **Scope** (optional) - The area affected (e.g., "parser", "lexer")
3. **Message** - A brief description of your change

Example commit: `feat(lexer): add support for hexadecimal literals`

### Step 4: Push Your Changes

The `normed_commit.sh` script will ask if you want to push immediately after committing. You can also push manually:

```bash
git push origin your-branch-name
```

### Step 5: Create a Pull Request

1. Go to the [GitHub repository](https://github.com/rune-prog-lang/glados)
2. Create a pull request targeting the `dev` branch
3. Fill in the PR description with details about your changes
4. Ensure your PR title follows the naming convention (e.g., `feat: add new feature`)

### Step 6: PR Requirements

Your PR must meet these requirements:

- **Naming Convention**: PR title must follow the conventional commit format (e.g., `fix: resolve parser bug`, `feat: add new function`)
  - This is a **mandatory** requirement checked by automated tests
  - The format is: `<type>(<optional scope>): <description>`

- **Automated Tests**: GitHub Actions workflows will automatically run:
  - Unit tests
  - Code linting and formatting checks
  - Build verification

Wait for the CI/CD pipeline to complete. All checks must pass before merging.

### Step 7: Review and Merge

Once your PR:
- Passes all automated tests
- Receives approval from maintainers
- Has a properly formatted title

It will be merged into the `dev` branch.

## Tips for Contributors

- **Keep commits atomic** - Each commit should represent a single logical change
- **Write clear commit messages** - Use the scope field to indicate which component you're modifying
- **Test your changes** - Run the build locally before pushing:
  ```bash
  make
  ```
- **Check the documentation** - Update relevant documentation in the `docs/` directory if needed
- **Review the existing code** - Familiarize yourself with the project structure (Lisp and Rune components)

## Project Structure

- `LispLang/` - Lisp interpreter implementation
- `RuneLang/` - Rune language interpreter and compiler
- `scripts/` - Utility scripts for development
- `docs/` - Project documentation

## Need Help?

- Check the README.md for general information
- Review the docs/ directory for architectural details
- Look at existing pull requests and commits for examples

Happy contributing! 🚀
