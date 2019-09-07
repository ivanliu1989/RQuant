#' Add Commit and Push Cuurent Git Repository
#'
#' Add Commit and Push Cuurent Git Repository
#'
#' @param gitURL the SSH URL of git repository
#' @param initRepo initialise the git repo or not
#' @param branch default "master"
#' @param username username of git account
#' @param commitMsg commit message
#' @param repoPath local path of repository
#'
#' @examples
#' pushGitRepo(gitURL = "git@github.com:ivanliu1989/RQuant.git",
#' branch = "master",
#' commitMsg = "update repo",
#' repoPath = getwd())
#'
#' @export
pushGitRepo <- function(gitURL = "git@github.com:ivanliu1989/RQuant.git",
                        initRepo = FALSE,
                        branch = "master",
                        username = "ivanliu1989",
                        commitMsg = "update repo",
                        repoPath = getwd()){

    system(paste0("cd ", repoPath))
    system("ls")
    if(initRepo){
        system("git init")
        system(paste0("git remote add origin ", gitURL))
        system(paste0("git remote set-url origin git@github.com:",username,"/repo.git"))
        system("git add .")
        system(paste0("git commit -m \"", commitMsg, "\""))
        system("git push -u origin master")
    }
    system("git add .")
    system(paste0("git commit -m \"", commitMsg, "\""))
    system(paste0("git push origin ", branch))

    # SSH
    # system("ls -al ~/.ssh")
    # system("ssh-keygen -t rsa -b 4096 -C \"ivan.liuyanfeng@gmail.com\"")

    # Config
    # system("git config --global user.name \"Ivan Liu\"")
    # system("git config --global user.email \"ivan.liuyanfeng@gmail.com\"")

    # Delete
    # unlink("./.git/", recursive = TRUE)

    # Read Key
    # readLines("~/.ssh/id_rsa.pub", n = 10)
}



