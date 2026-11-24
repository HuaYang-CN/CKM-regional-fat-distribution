# Fit models only to the current NHANES public-use analytic domain.
run_associations <- function(nh, raw_dir, out, loading_file) {
  regional_vars <- c("BMI","VAT_area","Trunk_pct","LA_pct","RA_pct","LL_pct","RL_pct")
  weights <- read.csv(loading_file)
  stopifnot(identical(weights$variable,regional_vars), !anyDuplicated(nh$SEQN))
  regional <- as.matrix(nh[regional_vars])
  local <- orient_pca(prcomp(regional,center=TRUE,scale.=TRUE),"VAT_area",c("LL_pct","RL_pct"))
  fixed <- scale(regional) %*% as.matrix(weights[c("PC1","PC2")])
  weighted_z <- function(x) {
    ms <- weighted_mean_sd(x,nh$WTSAF8YR)
    (x-ms["mean"])/ms["sd"]
  }
  nh$fixed1 <- weighted_z(fixed[,1]); nh$fixed2 <- weighted_z(fixed[,2])
  nh$local1 <- weighted_z(local$x[,1]); nh$local2 <- weighted_z(local$x[,2])
  nh$sex_f <- factor(nh$sex); nh$smoke_f <- factor(nh$current_smoker)
  nh$stage <- nh$ckm_stage_primary_revised
  nh$advanced <- as.integer(nh$advanced_ckm_primary_revised)
  nh$age10 <- (nh$age-mean(nh$age))/10
  read_alq <- function(suffix) {
    x <- haven::read_xpt(file.path(raw_dir,paste0("ALQ_",suffix,".XPT")))
    if (suffix == "J") {
      use <- classify_alq_2017_18(x,"ALQ_J")
    } else {
      f <- as.numeric(x$ALQ120Q)
      use <- dplyr::case_when(f==0 ~ FALSE,f>0 & !f %in% c(777,999) ~ TRUE,TRUE ~ NA)
    }
    skipped <- if(suffix=="J") x$ALQ111 %in% 2 & is.na(x$ALQ121) else rep(FALSE,nrow(x))
    data.frame(SEQN=x$SEQN, alcohol_use=use, abstainer_skip=skipped)
  }
  alq <- do.call(rbind,lapply(c("G","H","I","J"),read_alq))
  stopifnot(!anyDuplicated(alq$SEQN))
  nh$alcohol <- as.numeric(alq$alcohol_use[match(nh$SEQN,alq$SEQN)])
  skip <- alq$abstainer_skip[match(nh$SEQN,alq$SEQN)] %in% TRUE
  stopifnot(all(nh$alcohol[skip]==0))
  design <- function(x, wt=~WTSAF8YR) {
    survey::svydesign(ids=~PSU_8YR,strata=~STRATA_8YR,weights=wt,data=x,nest=TRUE)
  }
  full <- design(nh)
  good <- complete.cases(nh[c("advanced","sex_f","smoke_f","fixed1","fixed2","local1","local2")])
  full <- subset(full,good)
  restricted <- subset(full, stage>=2)
  alcohol <- design(nh[good & !is.na(nh$alcohol),,drop=FALSE])
  fit <- function(id, des, fixed=TRUE, extra=character()) {
    pc <- if(fixed) c("fixed1","fixed2") else c("local1","local2")
    model <- survey::svyglm(reformulate(c(pc,"sex_f","smoke_f",extra),"advanced"),
                            design=des,family=quasibinomial())
    tab <- extract_svy_or(model,des,pc[2])
    data.frame(model=id,N=nrow(des$variables),advanced=sum(des$variables$advanced),OR=tab$OR,
       lower=tab$Lower,upper=tab$Upper,p=tab$P,t=unname(coef(summary(model))[pc[2],3]),
       residual_df=model$df.residual,design_df=survey::degf(des))
  }
  fits <- list(fit("primary",full),fit("age",full,extra="age10"),fit("stage_2_4",restricted),
    fit("local",full,FALSE),fit("C07",alcohol),fit("C08",alcohol,extra="alcohol"),
    fit("C09",alcohol,FALSE),fit("C10",alcohol,FALSE,"alcohol"),fit("C11",subset(alcohol,stage>=2),extra="alcohol"))
  for (cy in sort(unique(as.character(nh$cycle)))) {
    x <- nh[as.character(nh$cycle)!=cy,,drop=FALSE]
    x$LOCO_WT <- x$WTSAF2YR/length(unique(as.character(x$cycle)))
    fits[[length(fits)+1]] <- fit(paste0("omit_",cy),design(x,~LOCO_WT))
  }
  write_tsv <- function(x,file) write.table(x,file.path(out,file),sep="\t",row.names=FALSE,quote=FALSE)
  write_tsv(do.call(rbind,fits),"models.tsv")
  write_tsv(as.data.frame(table(stage=nh$stage)),"stage_counts.tsv")
  write_tsv(data.frame(metric=c("fixed_local_score_r","ALQ_J_abstainer_skips","drinkers"),
      value=c(cor(nh$fixed2,nh$local2),sum(skip),sum(nh$alcohol,na.rm=TRUE))),"diagnostics.tsv")
}
