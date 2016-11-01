module Fake.DockerHelper

open Fake

type DockerImageName =
  { Host : string option
    Repository : string
    Name : string option
    Tag : string option
  }
  override x.ToString () =
    let h = defaultArg (x.Host |> Option.map (fun h -> h + "/")) ""
    let t = defaultArg (x.Tag |> Option.map (fun t -> ":" + t)) ""
    let n = defaultArg (x.Name |> Option.map (fun n -> "/" + n)) ""
    sprintf "%s%s%s%s" h x.Repository n t

type DockerInstance = DockerInstance of string

type DockerCommand =
  | Build of DockerImageName * root:string * dockerfile:string option
  | Run of DockerImageName * exposePort:uint16 * volumes:(string * string) list
  | Inspect of DockerInstance * path:string
  | Stop of DockerInstance
  | Remove of DockerInstance
  | RemoveImage of DockerImageName
  | Tag of image:DockerImageName * newName:DockerImageName
  | Push of DockerImageName
  | Login of host:string * username:string * password:string
  | Logout of host:string
  | CustomExec of cmd:string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DockerCommand =
  let toArgs = function
    | Build (i,r,d) -> sprintf "build --pull -t %O %s %s" i (defaultArg (Option.map ((+) "-f ") d) "") r
    | Run (i,p,vols) -> sprintf "run -d -p %i %s %O" p  (List.map (fun (l,r) -> sprintf "-v %s:%s" l r) vols |> String.concat " ") i
    | Inspect (DockerInstance i,p) -> sprintf "inspect -f '%s' %s" p i
    | Stop (DockerInstance i) -> sprintf "stop %s" i
    | Remove (DockerInstance i) -> sprintf "rm -f %s" i
    | RemoveImage i -> sprintf "rmi -f %O" i
    | Tag (i,n) -> sprintf "tag %O %O" i n
    | Push i -> sprintf "push %O" i
    | Login (h,u,p) -> sprintf "login %s -u %s -p %s" h u p
    | Logout h -> sprintf "logout %s" h
    | CustomExec cmd -> cmd

  let describe = function
    | Build (i,r,d) -> sprintf "Building container image: %O from context \"%s\" using %s" i r (defaultArg d "the Dockerfile in that directory")
    | Run (i,_,_) -> sprintf "Runing container: %O" i
    | Inspect (i,p) -> sprintf "Inspecting container metadata (%A: '%s')" i p
    | Stop i -> sprintf "Stoping container instance: %A" i
    | Remove i -> sprintf "Removing container instance: %A" i
    | RemoveImage i -> sprintf "Removing container image: %O" i
    | Tag (i,n) -> sprintf "Tagging container image: %O -> %O" i n
    | Push i -> sprintf "Pushing container image: %O" i
    | Login (h,u,p) -> sprintf "Setting docker credentials for %s@%s" u h
    | Logout h -> sprintf "Removing docker credentials on %s" h
    | CustomExec cmd -> sprintf "Executing %s" cmd

  let private timeoutMins = float >> System.TimeSpan.FromMinutes
  let private timeoutSecs = float >> System.TimeSpan.FromMinutes

  let timeout = function
    | Build _ -> timeoutMins 15
    | Run _ -> timeoutMins 15
    | Inspect _ -> timeoutSecs 15
    | Stop _ -> timeoutSecs 15
    | Remove _ -> timeoutSecs 30
    | RemoveImage _ -> timeoutSecs 30
    | Tag _ -> timeoutSecs 15
    | Push _ -> timeoutMins 15
    | Login _ -> timeoutSecs 15
    | Logout _ -> timeoutSecs 15
    | CustomExec _ -> timeoutMins 15

let docker (exec : (System.Diagnostics.ProcessStartInfo -> unit) -> System.TimeSpan -> 'a) (handler : 'a -> 'b) command =
  printfn "%s" <| DockerCommand.describe command
  exec
    ( fun psi -> psi.FileName <- "docker"; psi.Arguments <- DockerCommand.toArgs command )
    ( DockerCommand.timeout command )
  |> handler

let dockerDo command =
  let handler successful =
    if not successful then
      failwithf "Error - %s" (DockerCommand.describe command)
  docker execProcess handler command

let dockerAsk command =
  let handler (result : ProcessResult) =
    if result.OK then
      printfn "%s" <| String.concat "\n" result.Messages
    else
      failwithf
        "Error - %s\nOutput:%s\nErrors:%s"
        (DockerCommand.describe command)
        (String.concat "\n" result.Messages)
        (String.concat "\n" result.Errors)
    result
  docker ExecProcessAndReturnMessages handler command

let readFirstLine (result : ProcessResult) =
  result.Messages |> Seq.head

let dockerRun exposePort image volumes =
  (image, exposePort, volumes)
  |> Run
  |> dockerAsk
  |> readFirstLine
  |> DockerInstance

let dockerGetHostPort (exposedPort : uint16) inst =
  let path = sprintf """{{(index (index .NetworkSettings.Ports "%i/tcp") 0).HostPort}}""" exposedPort
  (inst, path)
  |> Inspect
  |> dockerAsk
  |> readFirstLine
  |> System.UInt16.Parse

let dockerTagPushRm image tag =
  dockerDo (Tag (image, tag))
  try
    dockerDo (Push tag)
  finally
    dockerDo (RemoveImage tag)
