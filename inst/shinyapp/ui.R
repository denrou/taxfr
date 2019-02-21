header <- dashboardHeader(title = "France inequalities")

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Map", icon = icon("map"), tabName = "map")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "map",
            mod_map_ui("mod_map")
        )
    )
)

dashboardPage(header, sidebar, body)
