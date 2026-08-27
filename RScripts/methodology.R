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
            #"<ul>",<li>
            "<a href='https://www.imls.gov/research-evaluation/surveys/public-libraries-survey-pls'>Institute for Museum and Library Services</a><br>", #</li>
            "<a href='https://www.air.org/project/helping-public-libraries-survey-create-reliable-methods-data-collection'>American Institute for Research</a><br>",
            "<a href='https://usdataexplorer.com/state/utah/'>Census Data Explorer</a><br>",
            "<a href='https://ut.countingopinions.com/'>LibPAS Homepage</a><br>"
          )
        )
      )
    ),
    col_widths = c(8, 4),
    card(
      card_header(
        "The Public Library Survey (PLS)",
        class = "my-header-grey",
      ),
      p(
        HTML(
          paste0(
            "<b>About</b><br>",
            "The PLS is an annual survey of libraries across the nation in the effort to gather data on library usage, services, finances, and operations. This information supports stakeholders and policymakers in making informed decisions about library resources and management. See the <a href= 'https://www.imls.gov/research-evaluation/surveys/public-libraries-survey-pls'>Institute for Museum and Library Services </a> (IMLS) website for more information.<br><br>",
            "<b>Methods</b><br> In coordination with IMLS, State Data Coordinators manage and disperse the PLS to the libraries in their respective states. These data are then compiled by IMLS and the American Institutes for Research (AIR) into a national dataset of library statistics.<br><br>",
            "<b>PLS Data in This Dashboard</b><br> IMLS data is used whenever possible, and unprocessed Utah data for the most recent year is used if the IMLS version is unavailable. Libraries were removed from the dataset if they were categorized as Survey Nonresponders, Permanently Closed, Removed (out of scope), or Future Library (FSCS ID Request). Data from Utah's four bookmobiles are not yet available. Puerto Rico was not included in this dashboard due to missing data and issues with the data encoding. 
            "
          )
        )
      )
    ),
    card(
      card_header(
        "About the Data",
        class = "my-header-grey",
      ),
      p(
        HTML(
          paste0(
            "<b>Peer Libraries</b><br>",
            "Using nationwide IMLS data from the most recent year available (",
            imls_year,
            "), peer libraries were identified for each library in the dataset. Statewide peers for Utah libraries use ",
            current_year,
            " data. Peers are the 10 most similar libraries to a given library based on five data points: number of visits, number of card holders, population of legal service area, total FTE, and total revenue. Similarity was calculated using Euclidean distance.<br><br>",
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
