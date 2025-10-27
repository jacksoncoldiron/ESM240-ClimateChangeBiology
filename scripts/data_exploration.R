# CMIP6 File Metadata & Variable Plot

# Load libraries
library(terra)
library(maps)

#### 1. Load NetCDF file ####
# Change this to YOUR downloaded file
f <- here::here("data", "tos_Omon_GFDL-ESM4_ssp585_r1i1p1f1_gr_201501-203412.nc")

# Lazy connection to the NetCDF file (does not load everything into memory)
r <- rast(f)

#### 2. Detect the variable ####
# Short variable name(s) in the NetCDF (e.g., "tos", "thetao", etc.)
vn <- tryCatch(terra::varnames(r), error = function(e) NA_character_)
# Use the first variable stack in the file (keeps this example generic)
r_var <- r[[1]]

#### 3. Print key metadata ####
cat("\n================== METADATA ==================\n")
print(r)

# Parse common CMIP6 filename parts
fname   <- basename(f)
parts   <- unlist(strsplit(fname, "_"))

model    <- if (length(parts) >= 3) parts[3] else NA_character_
scenario <- if (length(parts) >= 4) parts[4] else NA_character_
ensemble <- if (length(parts) >= 5) parts[5] else NA_character_

# Units may not always be present—handle gracefully
u <- tryCatch(units(r_var)[1], error = function(e) "unknown")

cat("\nModel name:", model,
    "\nEnsemble member:", ensemble,
    "\nScenario:", scenario,
    "\nNative resolution (lon x lat, degrees):", paste(terra::res(r_var), collapse = " x "),
    "\nVariable name(s):", if (is.null(vn)) "unknown" else paste(vn, collapse = ", "),
    "\nVariable units:", u,
    "\n=============================================\n\n")

#### 4. Plot (quick look) ####
# If a time dimension exists, take the first layer
if (nlyr(r_var) > 1) r_var <- r_var[[1]]

# Rotate longitudes to [-180, 180] for a clean world map
r_plot <- terra::rotate(r_var)

# Try to extract a timestamp (may be NULL depending on file)
tstamp <- tryCatch(as.character(terra::time(r_plot)[1]), error = function(e) "")

# Save a quick PNG to your project folder
png("quick_var.png", width = 1000, height = 600, res = 120)
plot(r_plot, main = paste("Quick Plot -", ifelse(is.null(vn), "variable", vn[1]), tstamp))
maps::map("world", add = TRUE)
dev.off()

#### 5. Compute the global mean ####
mean_val <- terra::global(r_plot, "mean", na.rm = TRUE)[[1]]

cat("Variable plotted:", ifelse(is.null(vn), "unknown", vn[1]), "\n")
cat("Global mean value (first time step):", round(mean_val, 3), u, "\n")
cat("PNG saved:", normalizePath("quick_var.png"), "\n")