#' @import magrittr
#' @import ggplot2
#' 
#' @export
plot_connectivity <- function(ctrl, cond, mean_correct, cv_r_bg, cv_t_bg,  mods_cv_r, mods_cv_t, mod_names) {
    
   # Wes Anderson: Darjeeling
   # Blue Red Teal Orange
   	palette <- c("#5BBCD6", "#FF0000", "#00A08A", "#F2AD00")
	r_name <- ctrl
    t_name <- cond

    r_bg_name <- paste(r_name, "(BG)")
    t_bg_name <- paste(t_name, "(BG)")

	mapply(function(cv_r, cv_t, mod_name){
		df <- c()		   
		if(!mean_correct) {
        	df <- data.frame(Connectivity = c(cv_r, cv_t),
                         	Group = factor(c(rep(r_name, length(cv_r)),
										  	rep(t_name, length(cv_t))),
											levels = c(r_name, t_name)))
        
		else {
            r_bg <- unlist(cv_r_bg)
            t_bg <- unlist(cv_t_bg)
                
            df <- data.frame(Connectivity = c(r_bg, t_bg),
                             Group = factor(c(rep(r_bg_name, length(r_bg)),
											  rep(t_bg_name, length(t_bg))),
											levels = c(r_bg_name, t_bg_name)))
        } 

        df %>% 
        ggplot(aes(y=Connectivity, x=Group, fill=Group)) +
        geom_boxplot() +
        theme_minimal() +
        theme(legend.position="none") + 
        ylab("Measured Connectivity") +
        ggtitle(mod_name) +
        theme(axis.text.x=element_text(angle=30))+
        scale_fill_manual(values=palette)

    }, mods_cv_r, mods_cv_t, mod_names, SIMPLIFY=FALSE)
}

#' @import magrittr
#' @import ggplot2
#' 
#' @export
plot_permutations <- function(mdc_type, mod_names, iter, mods_mdc_adj) {
    mdc_type <- mdc_type
    mdc_permutated <- data.frame(iter) %>%
                      set_colnames(mod_names)
    
    mapply(function(mdc_permuted, mdc_value, mod_name) {
        
        df <- data.frame(permutations=mdc_permuted)
        
        if (mdc_type == "fraction") {
            color <- ifelse(mdc_value > 1, "#FF0000", "#5BBCD6")    
        }
        if (mdc_type == "difference") {
            color <- ifelse(mdc_value > 0, "#FF0000", "#5BBCD6")
        }
        
        df %>%
        ggplot(aes(x=permutations)) +
        theme_minimal() +
        geom_density(color="white", fill=color) +
        ggtitle(mod_name) +
        ylab("Probability Density") +
        xlab("Permutated Differential Connectivity") +
        geom_vline(xintercept=mdc_value, linetype="dotted", size=1) +
        theme(axis.title.x=element_text(vjust=-1))
        
    }, mdc_permutated, mods_mdc_adj, mod_names, SIMPLIFY=FALSE)
}
