sidebar_MonitoR <- bs4DashSidebar(
  
  
  skin = "dark",
  status = "light",
  title = "RStudio Table Contest ",
  brandColor = "light",
  src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/724px-R_logo.svg.png",
  expand_on_hover = F,  
  fixed = F,
  
  bs4SidebarMenu(flat = F, compact = T,
    bs4SidebarMenuItem(
      "Survival Table",
      tabName = "Tab1",
      icon = "sliders"
    )
  ), includeHTML('./www/description.html')
  
)