module Api.Endpoints exposing
    ( BuildEndpoint(..)
    , Endpoint(..)
    , InstanceGroupEndpoint(..)
    , JobEndpoint(..)
    , PipelineEndpoint(..)
    , ResourceEndpoint(..)
    , ResourceVersionEndpoint(..)
    , TeamEndpoint(..)
    , toString
    )

import Concourse
import RouteBuilder exposing (RouteBuilder, append, appendPath, appendQuery)
import Url.Builder


type Endpoint
    = PipelinesList
    | Pipeline Concourse.PipelineIdentifier PipelineEndpoint
    | JobsList
    | Job Concourse.JobIdentifier JobEndpoint
    | JobBuild Concourse.JobBuildIdentifier
    | Build Concourse.BuildId BuildEndpoint
    | ResourcesList
    | Resource Concourse.ResourceIdentifier ResourceEndpoint
    | ResourceVersion Concourse.VersionedResourceIdentifier ResourceVersionEndpoint
    | TeamsList
    | Team Concourse.TeamName TeamEndpoint
    | ClusterInfo
    | Cli
    | UserInfo
    | Logout
    | InstanceGroup Concourse.InstanceGroupIdentifier InstanceGroupEndpoint
    | MaintenanceBanner


type PipelineEndpoint
    = BasePipeline
    | PausePipeline
    | UnpausePipeline
    | ExposePipeline
    | HidePipeline
    | PipelineJobsList
    | PipelineResourcesList


type JobEndpoint
    = BaseJob
    | PauseJob
    | UnpauseJob
    | JobBuildsList


type BuildEndpoint
    = BaseBuild
    | BuildPlan
    | BuildPrep
    | AbortBuild
    | BuildResourcesList
    | BuildEventStream
    | SetComment


type ResourceEndpoint
    = BaseResource
    | ResourceVersionsList
    | UnpinResource
    | CheckResource
    | PinResourceComment


type ResourceVersionEndpoint
    = BaseResourceVersion
    | ResourceVersionInputTo
    | DownstreamCausality
    | ResourceVersionOutputOf
    | UpstreamCasuality
    | PinResourceVersion
    | EnableResourceVersion
    | DisableResourceVersion


type TeamEndpoint
    = TeamPipelinesList
    | OrderTeamPipelines


type InstanceGroupEndpoint
    = OrderInstanceGroupPipelines


base : RouteBuilder
base =
    ( [ "api", "v1" ], [] )


baseSky : RouteBuilder
baseSky =
    ( [ "sky" ], [] )


pipeline :
    { r
        | pipelineName : String
        , pipelineInstanceVars : Concourse.InstanceVars
        , teamName : String
    }
    -> RouteBuilder
pipeline id =
    base |> append (RouteBuilder.pipeline id)


resource :
    { r
        | pipelineName : String
        , pipelineInstanceVars : Concourse.InstanceVars
        , teamName : String
        , resourceName : String
    }
    -> RouteBuilder
resource id =
    pipeline id |> appendPath [ "resources", id.resourceName ]


toString : Endpoint -> List Url.Builder.QueryParameter -> String
toString endpoint query =
    let
        rbToString : (List String, List Url.Builder.QueryParameter) -> String
        rbToString (paths, params) =
            let
                pathStr = Url.Builder.absolute paths []
                queryStr = Url.Builder.toQuery (params ++ query)
            in
            if queryStr == "" then
                pathStr
            else
                pathStr ++ queryStr
        path =
            case endpoint of
                PipelinesList ->
                    "/api/v1/pipelines"

                Pipeline id subEndpoint ->
                    rbToString (pipeline id |> append (pipelineEndpoint subEndpoint))

                JobsList ->
                    "/api/v1/jobs"

                Job id subEndpoint ->
                    rbToString (
                        pipeline id
                            |> appendPath [ "jobs", id.jobName ]
                            |> append (jobEndpoint subEndpoint)
                    )

                JobBuild id ->
                    rbToString (
                        pipeline id |> appendPath [ "jobs", id.jobName, "builds", id.buildName ]
                    )

                Build id subEndpoint ->
                    rbToString (
                        base
                            |> appendPath [ "builds", String.fromInt id ]
                            |> append (buildEndpoint subEndpoint)
                    )

                ResourcesList ->
                    "/api/v1/resources"

                Resource id subEndpoint ->
                    rbToString (resource id |> append (resourceEndpoint subEndpoint))

                ResourceVersion id subEndpoint ->
                    rbToString (
                        resource id
                            |> appendPath [ "versions", String.fromInt id.versionID ]
                            |> append (resourceVersionEndpoint subEndpoint)
                    )

                TeamsList ->
                    "/api/v1/teams"

                Team teamName subEndpoint ->
                    rbToString (
                        base
                            |> appendPath [ "teams", teamName ]
                            |> append (teamEndpoint subEndpoint)
                    )

                ClusterInfo ->
                    "/api/v1/info"

                Cli ->
                    "/api/v1/cli"

                UserInfo ->
                    "/api/v1/user"

                Logout ->
                    rbToString (baseSky |> appendPath [ "logout" ])

                InstanceGroup { teamName, name } subEndpoint ->
                    rbToString (
                        base
                            |> appendPath [ "teams", teamName ]
                            |> appendPath [ "pipelines", name ]
                            |> append (instanceGroupEndpoint subEndpoint)
                    )

                MaintenanceBanner ->
                    "/api/v1/maintenance-banner"
    in
    path ++ (if List.isEmpty query then "" else Url.Builder.toQuery query)


builder : Endpoint -> RouteBuilder
builder endpoint =
    case endpoint of
        PipelinesList ->
            base |> appendPath [ "pipelines" ]

        Pipeline id subEndpoint ->
            pipeline id |> append (pipelineEndpoint subEndpoint)

        JobsList ->
            base |> appendPath [ "jobs" ]

        Job id subEndpoint ->
            pipeline id
                |> appendPath [ "jobs", id.jobName ]
                |> append (jobEndpoint subEndpoint)

        JobBuild id ->
            pipeline id |> appendPath [ "jobs", id.jobName, "builds", id.buildName ]

        Build id subEndpoint ->
            base
                |> appendPath [ "builds", String.fromInt id ]
                |> append (buildEndpoint subEndpoint)

        ResourcesList ->
            base |> appendPath [ "resources" ]

        Resource id subEndpoint ->
            resource id |> append (resourceEndpoint subEndpoint)

        ResourceVersion id subEndpoint ->
            resource id
                |> appendPath [ "versions", String.fromInt id.versionID ]
                |> append (resourceVersionEndpoint subEndpoint)

        TeamsList ->
            base |> appendPath [ "teams" ]

        Team teamName subEndpoint ->
            base
                |> appendPath [ "teams", teamName ]
                |> append (teamEndpoint subEndpoint)

        ClusterInfo ->
            base |> appendPath [ "info" ]

        Cli ->
            base |> appendPath [ "cli" ]

        UserInfo ->
            base |> appendPath [ "user" ]

        Logout ->
            baseSky |> appendPath [ "logout" ]

        InstanceGroup { teamName, name } subEndpoint ->
            base
                |> appendPath [ "teams", teamName ]
                |> appendPath [ "pipelines", name ]
                |> append (instanceGroupEndpoint subEndpoint)

        MaintenanceBanner ->
            base |> appendPath [ "maintenance-banner" ]


pipelineEndpoint : PipelineEndpoint -> RouteBuilder
pipelineEndpoint endpoint =
    ( case endpoint of
        BasePipeline ->
            []

        PausePipeline ->
            [ "pause" ]

        UnpausePipeline ->
            [ "unpause" ]

        ExposePipeline ->
            [ "expose" ]

        HidePipeline ->
            [ "hide" ]

        PipelineJobsList ->
            [ "jobs" ]

        PipelineResourcesList ->
            [ "resources" ]
    , []
    )


jobEndpoint : JobEndpoint -> RouteBuilder
jobEndpoint endpoint =
    ( case endpoint of
        BaseJob ->
            []

        PauseJob ->
            [ "pause" ]

        UnpauseJob ->
            [ "unpause" ]

        JobBuildsList ->
            [ "builds" ]
    , []
    )


buildEndpoint : BuildEndpoint -> RouteBuilder
buildEndpoint endpoint =
    ( case endpoint of
        BaseBuild ->
            []

        BuildPlan ->
            [ "plan" ]

        BuildPrep ->
            [ "preparation" ]

        AbortBuild ->
            [ "abort" ]

        BuildResourcesList ->
            [ "resources" ]

        BuildEventStream ->
            [ "events" ]

        SetComment ->
            [ "comment" ]
    , []
    )


resourceEndpoint : ResourceEndpoint -> RouteBuilder
resourceEndpoint endpoint =
    ( case endpoint of
        BaseResource ->
            []

        ResourceVersionsList ->
            [ "versions" ]

        UnpinResource ->
            [ "unpin" ]

        CheckResource ->
            [ "check" ]

        PinResourceComment ->
            [ "pin_comment" ]
    , []
    )


resourceVersionEndpoint : ResourceVersionEndpoint -> RouteBuilder
resourceVersionEndpoint endpoint =
    ( case endpoint of
        BaseResourceVersion ->
            []

        ResourceVersionInputTo ->
            [ "input_to" ]

        ResourceVersionOutputOf ->
            [ "output_of" ]

        PinResourceVersion ->
            [ "pin" ]

        EnableResourceVersion ->
            [ "enable" ]

        DisableResourceVersion ->
            [ "disable" ]

        DownstreamCausality ->
            [ "downstream" ]

        UpstreamCasuality ->
            [ "upstream" ]
    , []
    )


teamEndpoint : TeamEndpoint -> RouteBuilder
teamEndpoint endpoint =
    ( case endpoint of
        TeamPipelinesList ->
            [ "pipelines" ]

        OrderTeamPipelines ->
            [ "pipelines", "ordering" ]
    , []
    )


instanceGroupEndpoint : InstanceGroupEndpoint -> RouteBuilder
instanceGroupEndpoint endpoint =
    ( case endpoint of
        OrderInstanceGroupPipelines ->
            [ "ordering" ]
    , []
    )
