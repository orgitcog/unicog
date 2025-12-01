# OpenCog Unified GitHub Actions Workflows

This directory contains GitHub Actions workflows for building, testing, and packaging the OpenCog cognitive architecture components.

## Build Workflows

### occ-build.yml - Ubuntu/Linux Build
**Status:** Production  
**Trigger:** Push to main/master, Pull Requests, Manual dispatch

Builds the complete OpenCog Collection (OCC) stack on Ubuntu Linux in dependency order:
1. CogUtil (Foundation layer)
2. AtomSpace (Knowledge representation)
3. AtomSpace Storage (Storage backends)
4. AtomSpace Rocks (RocksDB persistence)
5. CogServer (Network server)
6. AtomSpace Cog (Network storage)
7. Unify (Pattern matching)
8. URE (Unified Rule Engine)
9. Miner (Pattern mining)
10. Attention (ECAN attention allocation)
11. AS-MOSES (Evolutionary optimization)
12. Matrix (Sparse operations)
13. SpaceTime (Spatiotemporal reasoning)
14. PLN (Probabilistic Logic Networks)
15. Learn (Language learning)

**Features:**
- Dependency-ordered builds
- Caching for ccache and GHC
- Parallel builds with `-j2`
- Test execution with continue-on-error
- Artifact upload for build products

### ocwin-build.yml - Windows Build
**Status:** Experimental  
**Trigger:** Push to main/master, Pull Requests, Manual dispatch

Windows-specific build using Visual Studio 2022 and vcpkg for dependency management.

**Key Differences from Linux Build:**
- Uses PowerShell instead of Bash
- Dependencies via vcpkg and Chocolatey
- Visual Studio 2022 Build Tools
- MSBuild and CMake with Visual Studio generator
- Windows path separators (`\` instead of `/`)
- `.dll` and `.lib` artifacts instead of `.so`

**Supported Components:**
- CogUtil
- AtomSpace
- AtomSpace Storage
- AtomSpace Rocks
- CogServer

**Limitations:**
- No Guile Scheme bindings (Windows-specific issue)
- Some Unix-specific components may not build
- Haskell support requires Stack toolchain

## Packaging Workflows (Experimental)

### debian-packaging.yml - Debian Package Builder
**Status:** Experimental  
**Trigger:** Manual dispatch only

Creates `.deb` packages for Debian/Ubuntu systems.

**Parameters:**
- `component`: Which component to package (cogutil, atomspace, etc.)
- `version`: Package version (e.g., 1.0.0)

**Output:**
- `.deb` package files
- Installation instructions
- Lintian validation reports

**Usage:**
```bash
# After downloading the .deb files
sudo dpkg -i libcogutil-dev_1.0.0-1_amd64.deb
sudo apt-get install -f  # Fix dependencies
```

### apt-packaging.yml - APT Repository Builder
**Status:** Experimental  
**Trigger:** Manual dispatch only

Creates a complete APT repository structure for local hosting.

**Parameters:**
- `distribution`: Target distribution (focal, jammy, bookworm)
- `version`: Release version

**Output:**
- Complete APT repository archive (`.tar.gz`)
- Repository setup script
- Documentation

**Usage:**
```bash
# Extract and setup
sudo ./setup-opencog-repo.sh opencog-apt-repo-jammy.tar.gz

# Install packages
sudo apt-get update
sudo apt-get install libatomspace-dev
```

**Hosting Over Network:**
```bash
# Copy to web server
sudo cp -r /var/opencog-apt /var/www/html/opencog

# Add to sources.list
echo "deb [trusted=yes] http://your-server/opencog jammy main" | \
  sudo tee /etc/apt/sources.list.d/opencog.list
```

### chocolatey-packaging.yml - Windows Chocolatey Packages
**Status:** Experimental  
**Trigger:** Manual dispatch only

Creates Chocolatey `.nupkg` packages for Windows.

**Parameters:**
- `component`: Component to package
- `version`: Package version

**Output:**
- `.nupkg` Chocolatey package
- Installation guide
- Package metadata

**Usage:**
```powershell
# Install from local package
choco install opencog-cogutil -s . -y

# Or from Chocolatey server
choco source add -n=opencog -s="http://your-server/chocolatey"
choco install opencog-cogutil -y
```

### electron-packaging.yml - Electron Desktop Application
**Status:** Experimental  
**Trigger:** Manual dispatch only

Creates cross-platform desktop applications using Electron framework.

**Parameters:**
- `app_name`: Application name (e.g., OpenCog-Explorer)
- `version`: Application version

**Platforms:**
- Windows (`.exe` installer)
- Linux (AppImage, `.deb`, `.rpm`)
- macOS (`.dmg`) - requires macOS runner

**Features:**
- Web-based UI for AtomSpace exploration
- REST API backend (Express, port 3000)
- WebSocket server for real-time updates (port 3001)
- AtomSpace browser
- Query interface
- Reasoning engine monitor

**Output:**
- Platform-specific installers
- User guide and documentation

## Other Workflows

### bootstrap.yml
Repository initialization and setup workflow.

### cognitive-*.yml
Various cognitive architecture monitoring and orchestration workflows.

### fixme-tracking.yml, todo-catalog-update.yml
Development tracking and technical debt management.

## Workflow Dependencies

### Build Order
```
cogutil (Foundation)
  ├─→ atomspace
  │    ├─→ atomspace-storage
  │    │    ├─→ atomspace-rocks
  │    │    └─→ atomspace-cog
  │    ├─→ cogserver
  │    │    ├─→ attention
  │    │    ├─→ spacetime
  │    │    └─→ learn
  │    ├─→ unify
  │    │    └─→ ure
  │    │         ├─→ miner
  │    │         ├─→ pln
  │    │         └─→ asmoses
  │    └─→ matrix
  └─→ moses
```

### System Requirements

**Linux (Ubuntu/Debian):**
- CMake 3.10+
- GCC 7+ or Clang 6+
- Boost 1.65+
- Guile 3.0 or 2.2
- Python 3.6+
- Optional: RocksDB, Asio, Octomap

**Windows:**
- Visual Studio 2022 Build Tools
- CMake 3.20+
- vcpkg for dependencies
- Chocolatey for system tools
- Boost (via vcpkg)
- No Guile support

## Running Workflows

### Via GitHub UI
1. Go to Actions tab
2. Select desired workflow
3. Click "Run workflow"
4. Fill in parameters
5. Click "Run workflow" button

### Via GitHub CLI
```bash
# Trigger Windows build
gh workflow run ocwin-build.yml

# Trigger Debian packaging
gh workflow run debian-packaging.yml \
  -f component=cogutil \
  -f version=1.0.0

# Trigger APT repository build
gh workflow run apt-packaging.yml \
  -f distribution=jammy \
  -f version=1.0.0

# Trigger Chocolatey packaging
gh workflow run chocolatey-packaging.yml \
  -f component=cogutil \
  -f version=1.0.0

# Trigger Electron app build
gh workflow run electron-packaging.yml \
  -f app_name=OpenCog-Explorer \
  -f version=1.0.0
```

## Artifacts

Workflows produce various artifacts that are retained for 1-90 days:

- **Build artifacts** (1 day): Compiled libraries and binaries
- **Packages** (30 days): .deb, .nupkg, installers
- **Repositories** (90 days): APT repository archives
- **Documentation** (30 days): Install guides and reports

## Caching Strategy

To improve build times, workflows cache:

- **ccache**: Compiler cache for faster C++ compilation
- **vcpkg**: Windows dependency packages
- **GHC/Stack**: Haskell compiler and dependencies
- **npm/node_modules**: Node.js dependencies

Caches are keyed by:
- Runner OS
- Component source file hashes
- Dependency manifests

## Troubleshooting

### Build Failures

**Check logs:**
```bash
gh run view <run-id> --log
```

**Common issues:**
- Missing dependencies: Check install steps
- Compilation errors: Review build output
- Test failures: Usually continue-on-error, not blocking

### Package Installation Issues

**Debian/Ubuntu:**
```bash
# Fix dependency issues
sudo apt-get install -f

# Check package info
dpkg -l | grep opencog
```

**Windows:**
```powershell
# Check Chocolatey logs
Get-Content C:\ProgramData\chocolatey\logs\chocolatey.log -Tail 50

# Reinstall with force
choco install opencog-cogutil -y --force
```

## Contributing

To modify workflows:

1. Edit YAML files locally
2. Validate syntax:
   ```bash
   yamllint .github/workflows/your-workflow.yml
   # or
   python3 -c "import yaml; yaml.safe_load(open('.github/workflows/your-workflow.yml'))"
   ```
3. Test with workflow_dispatch trigger
4. Submit PR with changes

## Best Practices

1. **Use caching** for dependencies and build artifacts
2. **Set appropriate timeouts** for long-running builds
3. **Use continue-on-error** for non-critical steps
4. **Upload artifacts** for debugging and distribution
5. **Document inputs** with clear descriptions
6. **Version all dependencies** explicitly
7. **Test locally** when possible (act, docker)

## Future Improvements

- [ ] Matrix builds for multiple OS/compiler combinations
- [ ] Nightly builds and releases
- [ ] Performance benchmarking
- [ ] Code coverage reporting
- [ ] Security scanning (CodeQL, Snyk)
- [ ] Docker container builds
- [ ] Snap package support
- [ ] Flatpak support
- [ ] HomeBrew formula updates

## Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [OpenCog Build Instructions](../../QUICK-START.md)
- [OpenCog Development Roadmap](../../DEVELOPMENT-ROADMAP.md)
- [Component Configuration](../../component-config.json)

## License

These workflows are part of the OpenCog Unified repository and are licensed under AGPL-3.0.
