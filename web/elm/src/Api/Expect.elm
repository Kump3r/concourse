module Api.Expect exposing (text)

import Http

text : Http.Expect String
text =
    Http.expectStringResponse (\response ->
        Ok response.body
    )
