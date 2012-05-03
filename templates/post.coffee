pageParent = %{show $ parN}

attachReplyForm = ->
    $("#replyForm").on "submit", (event) ->
        event.preventDefault()
        post = { parent: pageParent
               , name: $("#replyName").val()
               , url: $("#replyUrl").val()
               , text: $("#replyText").val()
               }
        $.post "/reply", JSON.stringify(post)


$(document).ready ->
    attachReplyForm()
