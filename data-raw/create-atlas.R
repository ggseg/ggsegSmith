library(ggsegExtra)
library(dplyr) # for cleaning the atlas data efficiently
library(tidyr) # for cleaning the atlas data efficiently

# The unique name of the atlas annot, without hemisphere in filename
annot_name <- "smith"

for(hemi in c("lh", "rh")){
  tmp <- here::here(sprintf("data-raw/smith/%s.fsaverage_Smith", hemi))
  freesurfer::mris_convert_annot(
    infile = file.path(freesurfer::fs_subj_dir(), "fsaverage", "surf", "rh.orig"),
    annot = sprintf("%s.label.gii", tmp),
    ext = ".annot",
    outfile = sprintf("%s.annot", tmp)
  )  
}

# bash
# sudo cp data-raw/smith/lh.fsaverage_smith.annot $FREESURFER_HOME/subjects/fsaverage/label/lh.smith.annot
# sudo cp data-raw/smith/rh.fsaverage_smith.annot $FREESURFER_HOME/subjects/fsaverage/label/rh.smith.annot



# You might need to convert the annotation file
# convert atlas to fsaverage5
lapply(c("lh", "rh"),
       function(x){
         mri_surf2surf_rereg(subject = "fsaverage",
                             annot = annot_name,
                             hemi = x,
                             output_dir = here::here("data-raw/fsaverage5/"))
       })


# Make  3d ----
# Something is failing
smith_3d <- make_aparc_2_3datlas(
  annot = annot_name,
  annot_dir = here::here("data-raw/fsaverage5/"),
  output_dir = here::here("data-raw/")
)
ggseg3d(atlas = smith_3d)

## fix atlas ----
# you might need to do some alteration of the atlas data,
# like cleaning up the region names so they do not contain
# hemisphere information, and any unknown region should be NA
smith_n <- smith_3d
smith_n <- unnest(smith_n, ggseg_3d)
smith_n <- mutate(smith_n,
                    region = gsub("_L$|_R$", "", region),
                    region = ifelse(grepl("Unknown|\\?", region, ignore.case = TRUE), 
                                    NA, region),
                    atlas = "smith_3d"
)
smith_3d <- as_ggseg3d_atlas(smith_n)
ggseg3d(atlas  = smith_3d)


# Make palette ----
brain_pals <- make_palette_ggseg(smith_3d)
usethis::use_data(brain_pals, internal = TRUE, overwrite = TRUE)
devtools::load_all(".")


# Make 2d polygon ----
smith <- make_ggseg3d_2_ggseg(smith_3d, output_dir = here::here("data-raw/"))

plot(smith)

smith %>%
  ggseg(atlas = ., show.legend = TRUE,
        colour = "black",
        mapping = aes(fill=region)) +
  scale_fill_brain("smith", package = "ggsegSmith", na.value = "black")


usethis::use_data(smith, smith_3d,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


# make hex ----
atlas <- smith

p <- ggseg(atlas = atlas,
           hemi = "left",
           view = "lateral",
           show.legend = FALSE,
           colour = "grey30",
           size = .2,
           mapping = aes(fill =  region)) +
  scale_fill_brain2(palette = atlas$palette) +
  theme_void() +
  hexSticker::theme_transparent()

lapply(c("png", "svg"), function(x){
  hexSticker::sticker(p,
                      package = "ggsegSmith",
                      filename = sprintf("man/figures/logo.%s", x),
                      s_y = 1.2,
                      s_x = 1,
                      s_width = 1.5,
                      s_height = 1.5,
                      p_family = "mono",
                      p_size = 10,
                      p_color = "grey30",
                      p_y = .6,
                      h_fill = "white",
                      h_color = "grey30"
  )
  
})

pkgdown::build_favicons(overwrite = TRUE)
