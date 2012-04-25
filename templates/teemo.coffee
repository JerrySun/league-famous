@unhide = (preview) ->
    item = document.getElementById(preview)
    if item
        item.className = if (item.className == 'hidden') then 'unhidden' else 'hidden'

downvote = (name, callback) -> $.post "/downvote/" + name, null, callback
upvote   = (name, callback) -> $.post "/upvote/" + name, null, callback
novote   = (name, callback) -> $.post "/novote/" + name, null, callback

$(document).ready ->
    $(".thumbs").on "click", ".thumbup", ->
        thumbs = $(this).parent()

        thumbs.removeClass("downvoted")
        thumbs.toggleClass("upvoted")

        if thumbs.hasClass("upvoted")
            upvote thumbs.data("name")
        else
            novote thumbs.data("name")

    $(".thumbs").on "click", ".thumbdown", ->
        thumbs = $(this).parent()

        thumbs.removeClass("upvoted")
        thumbs.toggleClass("downvoted")

        if thumbs.hasClass("downvoted")
            downvote thumbs.data("name")
        else
            novote thumbs.data("name")
