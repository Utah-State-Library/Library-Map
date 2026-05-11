##### National #####

nav_panel(
  title = tags$h5(class = "fw-bold", "Methodology"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 1; margin: 1;",
  height = "82vh",

  ## Main Body ##
  # layout_columns(
  #   class = "bg-body-secondary container-fluid align-self-center",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = "Resources",
      width = "20%",

      p(
        HTML(
          paste0(
            "<ul>",
            "<li> <a href='https://www.imls.gov/research-evaluation/surveys/public-libraries-survey-pls'>Institute for Museum and Library Services</a></li>",
            "<li> <a href='https://www.air.org/project/helping-public-libraries-survey-create-reliable-methods-data-collection'>American Institute for Research</a></li>",
            "<li> <a href='https://usdataexplorer.com/state/utah/'>Census Data Explorer</a></li>"
          )
        )
      )
    ),
    col_widths = c(8, 4),
    card(
      card_header(
        "About the Data",
        class = "my-header-grey",
      ),
      p(
        HTML(
          paste0(
            "<b>Public Library Survey (PLS)</b><br>",
            "The PLS is an annual survey conducted nationwide to gather data on public libraries. <br><br>",
            "<u>Methods:</u> State Data Coordinators collect data from the libraries in their state and submit that data to the Institute for Museum and Library Services (IMLS). IMLS compiles all data into nationwide dataset.<br><br>",
            "<u>PLS Data in This Dashboard:</u> IMLS data is used whenever possible, and unprocessed Utah data for the most recent year is used if the IMLS version is unavailable. Libraries were filtered out of the dataset if they were categorized as Survey Nonresponders, Permanently Closed, Removed (out of scope), or Future Library (FSCS ID Request). Puerto Rico was not included in this dashboard due to missing data and issues with the data encoding.
            " # <br><br><br>
          )
        )
      )
    ),
    card(
      card_header(
        "Calculations",
        class = "my-header-grey",
      ),
      p(
        HTML(
          paste0(
            "<b>Peer Libraries</b><br>",
            "Using nationwide IMLS data from the most recent year available (",
            imls_year,
            "), peer libraries were identified for each library in the dataset. Note that statewide peers for Utah libraries use ",current_year," data. Peer libraries are those that are most similar to a given library, with similarity being calculated using Euclidean Distance. Euclidean Distance allows us to calculate the distance between two sets of data points; the smaller the distance, the more similar the data are. Each library was represented in the calculation by five data points: number of visits, number of card holders, population of legal service area, total FTE, and total revenue. For each library the 10 most similar libraries were identified as peers.<br>",
            "Data points were centered to mitigate scale sensitivity. Distance was calculated in R using stats::dist().<br><br>",
            "<b>Per Capita and Per FTE</b><br>",
            "In order to compare libraries and states on even footing, it is useful to represent library statistics in terms of per capita or per FTE values. <br><br>",
            "<u>Per Capita</u> shows how much service or usage occurs per person served by a given library. The number of people served by a given library is based on the most recent Census population estimates of the municipalities or counties in the library's jurisdiction. In cases where a value is small, such as FTE, it is useful to represent the value in terms of hundreds or thousands of people, rather than per person. <br>",
            "<u>Per FTE</u> shows how much service or usage occurs per Full Time Equivalent (FTE). One FTE is equal to a full-time work week. FTE is not necessarily equal to the number of staff working at a library because some staff may be part-time.
            <br><br>",
            "<b>Medians</b><br>",
            "The median represents the middle point of a range of values. It is less sensitive to outliers than the average, making it a good measure of nationwide data.<br>",
            "On the Compare States page each state is treated as a single system, with values from each library contributing to their respective statewide total. The national median is then the median of these statewide values.<br>",
            "On pages showing library-level data, median comparisons represent the median value of libraries within a specific group (i.e., peer libraries, all libraries in a given state, and all libraries nationwide)."
          )
        )
      )
    )
  )
)
