# One-time setup (delete this file afterwards)

From your project folder (the one with the .Rproj), in Git Bash / terminal:

```bash
mkdir data R report
git mv "RSVA_exports_Nicto otherregions.xlsx" "RSVA_importsfromregionstoNic.xlsx" \
       "RSVB_nicaraguatoregions.xlsx" "RSVB_regionstonicaragua.xlsx" data/   # skip git mv -> use mv if untracked
mv nicaragua_rsv_transmission_maps_v5.R old_v5.R                              # keep or delete
# copy in R/nicaragua_rsv_transmission_maps.R, README.md, LICENSE, data/README.md, .gitignore from this package
git add -A
git commit -m "Reorganise as reproducible repository"
git branch -M main
git remote add origin https://github.com/Simon-Mufara/rsv-nicaragua-flow-maps.git
git push -u origin main
```
Create the empty repo `rsv-nicaragua-flow-maps` on github.com first (no README/licence, since you already have them).
