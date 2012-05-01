pagePlayer = "%{unName $ playerName player}"

attachCommentForm = ->
    $("#newcomment").on "submit", (event) ->
        event.preventDefault()
        post = { player: pagePlayer
               , name: $("#commentName").val()
               , url: $("#commentUrl").val()
               , text: $("#commentText").val()
               }
        $.post "/newpost", JSON.stringify(post)


$(document).ready ->
    attachCommentForm()

    
