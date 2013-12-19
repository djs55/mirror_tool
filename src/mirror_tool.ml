open Lwt
open Printf
open Github_t
open Cmdliner

let rec iter user page =
  let open Github.Monad in
  Github.User.repos ~user ~page () >>= fun repos ->
  let repo_names = List.map (fun repo ->
    repo.repo_name) repos in
  if List.length repos = 30 
  then iter user (page+1) >>= fun names -> return (repo_names @ names)
  else return repo_names

let get_repos user =
  Github.Monad.run (iter user 1)

let exists filename = 
  Lwt.catch 
    (fun () -> Lwt_unix.stat filename >>= fun _ -> Lwt.return true)
    (fun _ -> Lwt.return false)

let pull git_filename =
  (* git fetch *)
  Printf.printf "Fetching %s\n%!" git_filename;
  Lwt_unix.chdir git_filename >>= fun _ ->
  Lwt_process.exec ("",[|"git";"fetch"|]) >>= fun _ ->
  Lwt.return ()

let clone mirror_dir user repo =
  (* git clone --mirror git://github.com/user/repo *)
  let dir = Printf.sprintf "%s/%s" mirror_dir user in
  Lwt_process.exec ("",[|"mkdir";"-p";dir|]) >>= fun _ ->
  let github_url = Printf.sprintf "git://github.com/%s/%s" user repo in
  Lwt_unix.chdir dir >>= fun _ ->
  Lwt_process.exec ("",[|"git";"clone";"--mirror";github_url|]) >>= fun _ ->
  Lwt.return ()

let mirror_repo mirror_dir user repo =
  (* Mirror the repo into <mirror_dir>/<user>/<repo>.git *)
  (* or, pull the latest changes *)
  let git_filename = Printf.sprintf "%s/%s/%s.git" mirror_dir user repo in
  exists git_filename >>= fun already_cloned ->
  if already_cloned
  then pull git_filename
  else clone mirror_dir user repo

let main user mirror_dir =
  Lwt_main.run (
    get_repos user >>= fun repos ->
    Lwt_list.iter_s (fun repo -> mirror_repo mirror_dir user repo) repos)

let mirror_dir = 
  let doc = "Set the root directory for the mirroring" in
  let home = Unix.getenv "HOME" in
  let default = Printf.sprintf "%s/github_mirror" home in
  Arg.(value & opt string default & info ["m"; "mirror"] ~doc)

let user = 
  let doc = "Set the user whose repositories are to be mirrored" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"USER" ~doc)

let cmd = 
  let doc = "Mirror a github users repositories locally" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) mirrors all of the git repositories of a user of github. If
        the repository already exists it will fetch the latest changes. The
        repositories created are 'bare mirrors', and do not have a working
        copy";
    `S "BUGS"; `P "Report bugs on the github issues page";
    `S "SEE ALSO"; `P "$(b,git)(1)" ] in
  Term.(pure main $ user $ mirror_dir),
  Term.(info "mirror_tool" ~version:"0.1" ~doc ~man)

let _ =
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
