pageParent = %{show $ postNum (parent thread)}

attachReplyForm = ->
    $("#replyForm").on "submit", (event) ->
        event.preventDefault()
        post = { parent: pageParent
               , name: $("#replyName").val()
               , text: $("#replyText").val()
               }
        maybeUrl = $("#replyUrl").val()
        if maybeUrl != ""
            post.url = maybeUrl
        $.post "/reply", JSON.stringify(post), -> location.reload()


$(document).ready ->
    attachReplyForm()
