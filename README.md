# ShinyRhizome

A Shiny front end for `rv`, the command-line tool of [RhizoVision Explorer](https://github.com/predictivephenomics/RhizoVisionExplorer) 2.5.0. It measures root length, diameter, area and volume from scanned root images.

- **Preview:** tune the threshold and clean-up settings on one image and check the segmentation.
- **Batch:** run the same settings on a whole folder and export one CSV.
- **Dry mass:** load dry mass per sample to get specific root length, specific root area, root tissue density, fine-root fraction and root length density.

## Quick start (Windows)

1. Download `ShinyRhizome-<version>-win64.zip` from [Releases](../../releases) and extract it anywhere.
2. Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp("path/to/ShinyRhizome")`.
3. Under **Files**, set the folder with your scans and the output folder.

The zip contains a portable `rv-dist/rv.exe` with all the DLLs it needs, so you don't need MSYS2 or any PATH changes. It needs a 64-bit CPU with AVX2.

## Notes

- `rv` 2.5.0 calculates root length differently from the 2.0.x GUI. Don't mix results from the two versions within one dataset.
- With both *Remove specks* and *Fill holes* switched on, `rv` skips the speck filter. The app warns when both are on.

## Building rv.exe yourself

`rv` exists only in the upstream `main` branch, and that branch doesn't build against OpenCV 5.0, which MSYS2 ships. The forks [muteNut/cvutil](https://github.com/muteNut/cvutil) and [muteNut/RhizoVisionExplorer](https://github.com/muteNut/RhizoVisionExplorer) carry the fixes on the branch `opencv5-msys2`.

In an MSYS2 UCRT64 shell:

```bash
pacman -S --needed git make mingw-w64-ucrt-x86_64-{gcc,cmake,qt6-base,qt6-charts,opencv}
git clone -b opencv5-msys2 https://github.com/muteNut/cvutil
git clone -b opencv5-msys2 https://github.com/muteNut/RhizoVisionExplorer

cmake -S cvutil -B cvutil/build -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_CXX_COMPILER=g++ -DUSE_MIMALLOC=OFF -DCMAKE_INSTALL_PREFIX=$HOME/rve
cmake --build cvutil/build -j$(nproc) && cmake --install cvutil/build

cmake -S RhizoVisionExplorer -B RhizoVisionExplorer/build -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_CXX_COMPILER=g++ -DCMAKE_PREFIX_PATH=$HOME/rve
cmake --build RhizoVisionExplorer/build --target rv -j$(nproc)

# portable folder: rv.exe plus every DLL it loads
mkdir -p rv-dist && cp RhizoVisionExplorer/build/rv.exe rv-dist/
PATH="$HOME/rve/bin:$PATH" ldd rv-dist/rv.exe \
  | awk '$3 ~ /^\/(ucrt64|home)\// {print $3}' \
  | sort -u | xargs -r cp -t rv-dist/
```

Ninja doesn't work with cvutil's build files, so stick to `"MSYS Makefiles"`. For CPUs without AVX2, add `-DENABLE_AVX2_FMA=OFF` to both `cmake -S` lines.

## License

The app is GPL-3.0. `rv.exe` and cvutil are also GPL-3.0; `rv-dist/SOURCE.txt` in the release zip names the exact source commits. The other DLLs in `rv-dist` are unmodified files from MSYS2 packages, mainly Qt 6 and OpenCV, under their own licenses.
