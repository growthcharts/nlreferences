# import selected references
path <- file.path("data-raw/data")
libs <- c("nl1980", "nl1997", "nl2009", "nlhs", "preterm", "dscore")

refcodes <- character(0)
for (lib in libs) {
  files <- list.files(file.path(path, lib))
  for (file in files) {
    ref <- centile::import_rif(file.path(path, lib, file))
    s <- attr(ref, "study")
    refcode <- centile::make_refcode(s["name"], s["year"], s["yname"], s["sex"], s["sub"])
    refcodes <- c(refcodes, refcode)
    assign(refcode, ref)
  }
}

usethis::use_data(
  refcodes,
  nl_1980_bmi_female_, nl_1980_bmi_male_, nl_1980_wfh_female_nla,nl_1980_wfh_female_nlb,nl_1980_wfh_male_nla,
  nl_1980_wfh_male_nlb, nl_1980_wgt_female_, nl_1980_wgt_male_, nl_1997_bmi_female_ma, nl_1997_bmi_female_nl,
  nl_1997_bmi_female_tu, nl_1997_bmi_male_ma, nl_1997_bmi_male_nl, nl_1997_bmi_male_tu, nl_1997_hdc_female_ma,
  nl_1997_hdc_female_nl, nl_1997_hdc_female_tu, nl_1997_hdc_male_ma, nl_1997_hdc_male_nl, nl_1997_hdc_male_tu,
  nl_1997_hgt_female_ma, nl_1997_hgt_female_nl, nl_1997_hgt_female_tu, nl_1997_hgt_male_ma, nl_1997_hgt_male_nl,
  nl_1997_hgt_male_tu, nl_1997_hip_female_, nl_1997_hip_male_, nl_1997_lgl_female_, nl_1997_lgl_male_,
  nl_1997_shh_female_, nl_1997_shh_male_, nl_1997_sit_female_, nl_1997_sit_male_, nl_1997_wfh_female_maa,
  nl_1997_wfh_female_mab,nl_1997_wfh_female_nla,nl_1997_wfh_female_nlb,nl_1997_wfh_female_tua,nl_1997_wfh_female_tub,
  nl_1997_wfh_male_maa, nl_1997_wfh_male_mab, nl_1997_wfh_male_nla, nl_1997_wfh_male_nlb, nl_1997_wfh_male_tua,
  nl_1997_wfh_male_tub, nl_1997_wgt_female_ma, nl_1997_wgt_female_nl, nl_1997_wgt_female_tu, nl_1997_wgt_male_ma,
  nl_1997_wgt_male_nl, nl_1997_wgt_male_tu, nl_1997_whr_female_, nl_1997_whr_male_, nl_1997_wst_female_,
  nl_1997_wst_male_, nl_2009_bmi_female_ma, nl_2009_bmi_female_nl, nl_2009_bmi_female_tu, nl_2009_bmi_male_ma,
  nl_2009_bmi_male_nl, nl_2009_bmi_male_tu, nl_2009_hdc_female_ds, nl_2009_hdc_male_ds, nl_2009_hgt_female_ds,
  nl_2009_hgt_female_ma, nl_2009_hgt_female_nl, nl_2009_hgt_female_tu, nl_2009_hgt_male_ds, nl_2009_hgt_male_ma,
  nl_2009_hgt_male_nl, nl_2009_hgt_male_tu, nl_2009_wgt_female_ds, nl_2009_wgt_female_nl, nl_2009_wgt_male_ds,
  nl_2009_wgt_male_nl, nl_1976_bmi_female_hs, nl_1976_bmi_male_hs, nl_1976_wfh_female_hs, nl_1976_wfh_male_hs,
  nl_1976_wgt_female_hs, nl_1976_wgt_male_hs, nl_2010_hgt_female_hs, nl_2010_hgt_male_hs, nl_2012_hdc_female_25,
  nl_2012_hdc_female_26, nl_2012_hdc_female_27, nl_2012_hdc_female_28, nl_2012_hdc_female_29, nl_2012_hdc_female_30,
  nl_2012_hdc_female_31, nl_2012_hdc_female_32, nl_2012_hdc_female_33, nl_2012_hdc_female_34, nl_2012_hdc_female_35,
  nl_2012_hdc_female_36, nl_2012_hdc_male_25, nl_2012_hdc_male_26, nl_2012_hdc_male_27, nl_2012_hdc_male_28,
  nl_2012_hdc_male_29, nl_2012_hdc_male_30, nl_2012_hdc_male_31, nl_2012_hdc_male_32, nl_2012_hdc_male_33,
  nl_2012_hdc_male_34, nl_2012_hdc_male_35, nl_2012_hdc_male_36, nl_2012_hgt_female_25, nl_2012_hgt_female_26,
  nl_2012_hgt_female_27, nl_2012_hgt_female_28, nl_2012_hgt_female_29, nl_2012_hgt_female_30, nl_2012_hgt_female_31,
  nl_2012_hgt_female_32, nl_2012_hgt_female_33, nl_2012_hgt_female_34, nl_2012_hgt_female_35, nl_2012_hgt_female_36,
  nl_2012_hgt_male_25, nl_2012_hgt_male_26, nl_2012_hgt_male_27, nl_2012_hgt_male_28, nl_2012_hgt_male_29,
  nl_2012_hgt_male_30, nl_2012_hgt_male_31, nl_2012_hgt_male_32, nl_2012_hgt_male_33, nl_2012_hgt_male_34,
  nl_2012_hgt_male_35, nl_2012_hgt_male_36, nl_2012_wfh_female_, nl_2012_wfh_male_, nl_2012_wgt_female_25,
  nl_2012_wgt_female_26, nl_2012_wgt_female_27, nl_2012_wgt_female_28, nl_2012_wgt_female_29, nl_2012_wgt_female_30,
  nl_2012_wgt_female_31, nl_2012_wgt_female_32, nl_2012_wgt_female_33, nl_2012_wgt_female_34, nl_2012_wgt_female_35,
  nl_2012_wgt_female_36, nl_2012_wgt_male_25, nl_2012_wgt_male_26, nl_2012_wgt_male_27, nl_2012_wgt_male_28,
  nl_2012_wgt_male_29, nl_2012_wgt_male_30, nl_2012_wgt_male_31, nl_2012_wgt_male_32, nl_2012_wgt_male_33,
  nl_2012_wgt_male_34, nl_2012_wgt_male_35, nl_2012_wgt_male_36, gc_2019_dsc_female_, gc_2019_dsc_male_,
  nl_2014_dsc_female_25, nl_2014_dsc_female_26, nl_2014_dsc_female_27, nl_2014_dsc_female_28, nl_2014_dsc_female_29,
  nl_2014_dsc_female_30, nl_2014_dsc_female_31, nl_2014_dsc_female_32, nl_2014_dsc_female_33, nl_2014_dsc_female_34,
  nl_2014_dsc_female_35, nl_2014_dsc_female_36, nl_2014_dsc_female_40, nl_2014_dsc_male_25, nl_2014_dsc_male_26,
  nl_2014_dsc_male_27, nl_2014_dsc_male_28, nl_2014_dsc_male_29, nl_2014_dsc_male_30, nl_2014_dsc_male_31,
  nl_2014_dsc_male_32, nl_2014_dsc_male_33, nl_2014_dsc_male_34, nl_2014_dsc_male_35, nl_2014_dsc_male_36,
  nl_2014_dsc_male_40, ph_2023_dsc_female_25, ph_2023_dsc_female_26, ph_2023_dsc_female_27, ph_2023_dsc_female_28,
  ph_2023_dsc_female_29, ph_2023_dsc_female_30, ph_2023_dsc_female_31, ph_2023_dsc_female_32, ph_2023_dsc_female_33,
  ph_2023_dsc_female_34, ph_2023_dsc_female_35, ph_2023_dsc_female_36, ph_2023_dsc_female_40, ph_2023_dsc_male_25,
  ph_2023_dsc_male_26, ph_2023_dsc_male_27, ph_2023_dsc_male_28, ph_2023_dsc_male_29, ph_2023_dsc_male_30,
  ph_2023_dsc_male_31, ph_2023_dsc_male_32, ph_2023_dsc_male_33, ph_2023_dsc_male_34, ph_2023_dsc_male_35,
  ph_2023_dsc_male_36, ph_2023_dsc_male_40,
  internal = TRUE, overwrite = TRUE
)
