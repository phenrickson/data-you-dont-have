# Load the quarto package
library(quarto)
library(dplyr)

# Define the season and list of teams
season <- 2024
game_info = targets::tar_read("season_game_info")
team_info = 
  game_info |> 
  select(conference = home_conference, 
         division = home_division, 
         team = home_team, 
         id = home_id) |> 
  distinct()

teams =
  team_info |> 
  filter(conference == 'SEC') |>
  head(2) |>
  pull(team)

# Loop over each team and render the report
for (team in teams) {
  cat("Rendering report for team:", team, "\n")
  
  # Define the output file name with season and team
  output_file <- paste0(season, "_", gsub(" ", "_", team), ".html")
  
  # Render the Quarto report with specified parameters
  quarto_render(
    input = "team_report.qmd",
    output_file = output_file,
    execute_params = list(season = season, team = team)
  )
}
