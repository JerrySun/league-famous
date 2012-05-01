attachCommentForm = ->
    $("#newcomment").on "submit", (event) ->
        event.preventDefault()
        post = { player: "ceres"
               , name: $(this).children(".commentingname").val()
               , url: $(this).children(".commentingimg").val()
               , text: $(this).children(".commentingtext").val()
               }
        console.log post
        $.post "/newpost", JSON.stringify(post)


$(document).ready ->
    attachCommentForm()

    
