#' Create a light bootstrap theme shiny apps.
#'
#' @param bg bg
#' @param fg fg
#' @param primary color primary
#' @param secondary color secondary
#' @param success color success
#' @param info color info
#' @param warning color warning
#' @param danger color danger
#' @param font_scale font scale
#' @param enable_gradients gradients
#' @param enable_shadows shadows
#' @param base_font base font
#'
#' @importFrom bslib bs_theme bs_theme_update
#'
#' @return A bs_theme function.
#' @export
#'
theme_shiny <- function(bg = "#FFFEFE", 
                        fg = "#DE633C", 
                        primary = "#15b7d6", 
                        secondary = "#DE633C",
                        success = "#fd994e",
                        info = "#fd5072",
                        warning = "#5cadfd",
                        danger = "#c4a5fd",
                        base_font = "Noto Sans sans-serif", 
                        font_scale = 1.1,
                        enable_gradients = TRUE, 
                        enable_shadows = TRUE) {
  bs_theme(
    bg = bg,
    fg = fg,
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = primary,
    secondary = secondary,
    success = success,
    info = info,
    warning = warning,
    danger = danger,
    base_font = base_font,
    font_scale = font_scale
  ) %>% 
    bs_theme_update(
      `enable-gradients` = enable_gradients,
      `enable-shadows` = enable_shadows
      # spacer = spacer
    )
  
}


#' Create a dark bootstrap theme shiny apps.
#'
#' @inheritParams theme_shiny
#'
#' @importFrom bslib bs_theme bs_add_variables font_google
#'
#' @return A bs_theme function.
#' @export
#'
theme_dark_shiny <- function(bg = "#000000",
                             fg = "#15b7d6",
                             primary = "#DE633C",
                             secondary = "#15b7d6",
                             success = "#fd994e",
                             info = "#fd5072",
                             warning = "#5cadfd",
                             danger = "#c4a5fd",
                             base_font = "Noto Sans sans-serif",
                             font_scale = 1.1,
                             enable_gradients = TRUE,
                             enable_shadows = TRUE){
  bs_theme(
    bg = bg,
    fg = fg,
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = primary,
    secondary = secondary,
    success = success,
    info = info,
    warning = warning,
    danger = danger,
    base_font = base_font,
    font_scale = font_scale
   ) %>% 
    bs_theme_update(
      `enable-gradients` = enable_gradients,
      `enable-shadows` = enable_shadows
    )
    
  }


#' Insert a switch button light/dark mode.
#'
#' @param inputId id of input
#'
#' @importFrom shiny icon fluidRow div tags tagList
#' @importFrom glue glue
#'
#' @return A fluidPage object.
#' @export
switch_button_theme <- function(inputId) {
  tagList(fluidRow(div(style = "height:20px; width:15px;")),
          fluidRow(
            div(style = "height:50px; width:15px;"),
            div(
              class = "custom-control custom-switch",
              tags$input(
                id = inputId,
                type = "checkbox",
                onclick = glue::glue(
                  "Shiny.setInputValue('{inputId}', document.getElementById('{inputId}').value);"
                ),
                class = "custom-control-input",
                tags$label(icon("adjust"), `for` = inputId, class = "custom-control-label")
              ),
              div(style = "height:30px; width:15px;")
            )
          ),
          script_js(inputId))
}


#' Init cookie for theme
#'
#' @param input result of input switch button
#'
#' @importFrom glouton add_cookie fetch_cookie
#'
#' @return cookie inside browser
#' @export
#'
init_cookie_theme <- function(input) {
  cookie <- fetch_cookie('theme')
  if (length(cookie) == 0) {
    add_cookie("theme", input)
  }
}

#' Switch beetween dark and light theme
#'
#' @param input result of input switch button
#' @param session session
#'
#' @importFrom glouton add_cookie
#'
#' @export
#'
change_theme <-
  function(input,
           session = shiny::getDefaultReactiveDomain()) {
    add_cookie("theme", input)
    
    session$setCurrentTheme(if (isTRUE(input)) {
      theme_dark_shiny()
    } else {
      theme_shiny()
    })
  }

#' Add external ressources and theme for fluidPage in shiny
#'
#' @param id id of fluidpage
#' @param ... params form fluidPage
#'
#' @importFrom shiny fluidPage
#' @importFrom glouton use_glouton
#'
#' @return fluidpage
#' @export
fluidpage <- function(..., id = "dark_mode-page") {
  fluidPage(
    id = id,
    style = "display:none;",
    theme = theme_shiny(),
    use_glouton(),
    ...
  )
}


#' @describeIn fluidpage Nav bar page
#' @importFrom shiny navbarPage
navbarpage <- function(..., id = "dark_mode-page") {
  navbarPage(
    id = id,
    style = "display:none;",
    theme = theme_shiny(),
    ...,
    use_glouton()
  )
}

#' Js for switch button
#'
#' @param id id of fluidpage or navbarpage
#' @param input name of input
#'
#' @importFrom glue glue
#' @importFrom shiny tags
#'
#' @return tag script for switch button
#'
script_js <- function(input, id = "dark_mode-page") {
  tags$script(
    glue::glue(
      .open = '<',
      .close = '>',
      "$(document).on('shiny:sessioninitialized', function(event) {
         var res = Cookies.get('theme');
            if(res == 'true'){ $('#<input>').click()
                 Shiny.setInputValue('<input>', true)
            }else{
                Shiny.setInputValue('<input>', false)
            }

        setTimeout(function(){
          $('#<id>').css('display' , '')
          $('#<id>').trigger('show')
          $('#<id>').trigger('shown')}, 1000);

});"
    )
  )
}
