
# LIBRARIES --------------------------------------------------------------------

library(gh)
library(glue)
library(lubridate)
library(scales)
library(tidyverse)

# FUNCTIONS --------------------------------------------------------------------

#' Get issues
#'
#' Get issues for a repository
#'
#' @param repo Name of the repository (user/repo)
#'
#' @return list containing issues
get_issues <- function(repo) {
    message("Getting issues for ", repo, "...")
    endpoint <- glue("GET /repos/{repo}/issues")
    issues <- gh(endpoint, state = "all", .limit = Inf)
    message("Found ", length(issues), " issues")
    return(issues)
}

#' Get issue comments
#'
#' Get comments for a repository issue
#'
#' @param repo Name of the repository (user/repo)
#' @param issue Issue number
#'
#' @return list containing issue comments
get_issue_comments <- function(repo, issue) {
    message(glue("Getting comments for {repo} issue {issue}..."))
    endpoint <- glue("GET /repos/{repo}/issues/{issue}/comments")
    comments <- gh(endpoint, state = "all", .limit = Inf)
    message("Found ", length(comments), " comments")
    return(comments)
}

#' Get issue comments data frame
#'
#' Get comments for a repository issue as a data frame
#'
#' @param repo Name of the repository (user/repo)
#' @param issue Issue number
#'
#' @return tibble containing issue comments
get_comments_df <- function(repo, issue) {
    message(glue("Creating comments table for {repo} issue {issue}..."))

    comments <- get_issue_comments(repo, issue)

    if (length(comments) == 0) {
        return(
            tibble(
                ID      = numeric(),
                User    = character(),
                Created = ymd_hms(),
                Updated = ymd_hms()
            )
        )
    }

    comments_df <- map_dfr(comments, function(.comment) {
        tibble(
            ID      = .comment$id,
            User    = .comment$user$login,
            Created = .comment$created_at,
            Updated = .comment$updated_at
        )
    }) %>%
        arrange(Created)

    return(comments_df)
}

#' Get first response
#'
#' Get first response time for an issue
#'
#' @param comments_df data.frame containing issue comments
#' @param user Username for the user who created the issue
#' @param closed Time the issue was closed (or NA if still open)
#'
#' @return first response time
get_first_response <- function(comments_df, user, closed) {
    comments_df <- filter(comments_df, User != user)

    if (nrow(comments_df) > 0) {
        return(comments_df$Created[1])
    } else {
        return(closed)
    }
}

#' Get issues data frame
#'
#' Get issues for a repository as a data frame
#'
#' @param repo Name of the repository (user/repo)
#'
#' @return tibble containing issues
get_issues_df <- function(repo) {
    issues <- get_issues(repo)
    message("Creating issues table for ", repo, "...")

    if (length(issues) == 0) {
        return(
            tibble(
                ID       = numeric(),
                Number   = numeric(),
                PR       = logical(),
                State    = character(),
                Comments = numeric(),
                Created  = ymd_hms(),
                Response = ymd_hms(),
                Updated  = ymd_hms(),
                Closed   = ymd_hms()
            )
        )
    }

    issues_df <- map_dfr(issues, function(.issue) {
        closed <- .issue$closed_at
        if (is.null(closed)) {
            closed <- NA
        }
        PR <- !is.null(.issue$pull_request)
        tibble(
            ID       = .issue$id,
            Number   = .issue$number,
            User     = .issue$user$login,
            PR       = PR,
            State    = .issue$state,
            Created  = .issue$created_at,
            Updated  = .issue$updated_at,
            Closed   = closed
        )
    })
    message(glue("{sum(issues_df$PR)} are pull requests"))

    issues_df %>%
        filter(!PR) %>%
        select(-PR) %>%
        pmap_dfr(function(...) {
            row <- tibble(...)
            comments_df <- get_comments_df(repo, row$Number)
            row$Comments <- nrow(comments_df)
            row$Response <- get_first_response(comments_df, row$User,
                                               row$Closed)
            return(row)
        }) %>%
        mutate(Repo = repo) %>%
        relocate(Repo, ID, Number, User, State, Comments, Created, Response,
                 Updated, Closed) %>%
        mutate(
            across(c("Created", "Response", "Updated", "Closed"), as_datetime)
        )
}

#' Get repo
#'
#' Get information for a repository
#'
#' @param repo Name of the repository (user/repo)
#'
#' @return list containing repository information
get_repo <- function(repo) {
    message("Getting information for ", repo, "...")
    endpoint <- glue("GET /repos/{repo}")
    issues <- gh(endpoint)
    return(issues)
}

#' Get repo data frame
#'
#' Get information for a repository as a data frame
#'
#' @param repo Name of the repository (user/repo)
#'
#' @return tibble containing repository information
get_repo_df <- function(repo) {
    info <- get_repo(repo)
    tibble(
        Repo    = repo,
        Created = info$created_at,
        Updated = info$updated_at,
        Stars   = info$stargazers_count,
        Forks   = info$forks_count
    ) %>%
        mutate(
            across(c("Created", "Updated"), as_datetime)
        ) %>%
        mutate(AgeYears = as.numeric((now() - Created) / 365))
}

# MAIN CODE --------------------------------------------------------------------

repos <- c(
    "Teichlab/bbknn",
    "hms-dbmi/conos",
    "immunogenomics/harmony",
    "MacoskoLab/liger",
    "chriscainx/mnnpy",
    "brianhie/scanorama",
    "YosefLab/scVI",
    "satijalab/seurat",
    "theislab/trVAE",
    "jtleek/sva-devel",
    "eleozzr/desc",
    "KrishnaswamyLab/SAUCIE",
    "LTLA/batchelor",
    "theislab/scgen"
)

# Get the combined issues data frame for all repos
all_issues <- map_dfr(repos, get_issues_df)

# Summarise issues by repository
issues_summ <- all_issues %>%
    mutate(
        TimeResponse = Response - Created,
        TimeClosed   = Closed - Created
    ) %>%
    group_by(Repo) %>%
    summarise(
        Issues             = n(),
        Closed             = sum(State == "closed"),
        PctClosed          = Closed / Issues * 100,
        MedianResponseDays = as.numeric(median(TimeResponse, na.rm = TRUE)) /
                                 86400,
        MedianOpenDays     = as.numeric(median(TimeClosed, na.rm = TRUE)) /
                                 86400,
        .groups            = "drop"
    )

# Get combined data frame with all repo information
all_repos <- map_dfr(repos, get_repo_df)

# Calculate score for each repository
repo_summ <- all_repos %>%
    left_join(issues_summ, by = "Repo") %>%
    mutate(Tool = str_remove(Repo, "^[[:alnum:]-_]+/")) %>%
    relocate(Repo, Tool) %>%
    mutate(
        # Add 1 to avoid negative scores
        IssueActivity = log10((Closed / AgeYears) + 1),
        IssueResponse = 30 - MedianResponseDays
    ) %>%
    mutate(
        IssueActivity = if_else(
            is.na(IssueActivity),
            0.5 * min(IssueActivity, na.rm = TRUE),
            IssueActivity
        ),
        IssueResponse = if_else(
            is.na(IssueResponse),
            0.5 * min(IssueResponse, na.rm = TRUE),
            IssueResponse
        )
    ) %>%
    mutate(
        IssueActivityScore = rescale(IssueActivity,
                                     from = c(0, max(IssueActivity)),
                                     to = c(0, 1)),
        IssueResponseScore = rescale(IssueResponse, from = c(0, 30),
                                     to = c(0, 1)),
        IssueOverallScore  = 0.5 * (IssueActivityScore + IssueResponseScore)
    )

# Some basic plots as a sanity check
ggplot(repo_summ, aes(x = Repo, y = IssueActivityScore)) +
    geom_col() +
    coord_flip() +
    theme_minimal()

ggplot(repo_summ, aes(x = Repo, y = IssueResponseScore)) +
    geom_col() +
    coord_flip() +
    theme_minimal()

ggplot(repo_summ, aes(x = Repo, y = IssueOverallScore)) +
    geom_col() +
    coord_flip() +
    theme_minimal()

ggplot(repo_summ,
       aes(x = IssueActivityScore, y = IssueResponseScore, label = Tool)) +
    geom_text() +
    theme_minimal()

# Save the final summary
write_tsv(repo_summ, "../data/gh_repo_summary.tsv")
