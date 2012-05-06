pagePlayer = "%{unName $ statName player}"

attachCommentForm = ->
    $("#newcomment").on "submit", (event) ->
        event.preventDefault()
        post = { player: pagePlayer
               , name: $("#commentName").val()
               , text: $("#commentText").val()
               }
        maybeUrl = $("#commentUrl").val()
        if maybeUrl != ""
            post.url = maybeUrl
        $.post "/newpost", JSON.stringify(post), -> location.reload()


$(document).ready ->
    attachCommentForm()

    
