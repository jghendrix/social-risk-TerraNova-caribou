#' @title Summarise and check model
#' @export
#' @author Jack G Hendrix
summarise_model <- function(model, DT) {

	print(unique(DT$season))
	print(summary(model))
	print(performance::check_collinearity(model))

}
