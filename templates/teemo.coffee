@unhide = (preview) ->
    item = document.getElementById(preview)
    if item
        item.className = if (item.className == 'hidden') then 'unhidden' else 'hidden'

reloadTable = -> $.ajax { url: "/table"
                        , success: (x) -> $("#players").replaceWith x; attachRow() }

downvote = (name, callback) -> $.post "/downvote/" + name, null, reloadTable
upvote   = (name, callback) -> $.post "/upvote/" + name, null, reloadTable
novote   = (name, callback) -> $.post "/novote/" + name, null, reloadTable

replacePreview = (x) ->
    $("#preview").replaceWith x
    $("#preview").show()
    closeButton = $("#preview").children(".previewclose")
    closeButton.click(-> $("#preview").hide())

attachRow = ->
    $(".playerrow").on "click", ".thumbup", ->
        thumbs = $(this).parent()

        thumbs.removeClass("downvoted")
        thumbs.toggleClass("upvoted")

        name = $(this).closest(".playerrow").data("name")

        if thumbs.hasClass("upvoted")
            upvote name
        else
            novote name

    $(".playerrow").on "click", ".thumbdown", ->
        thumbs = $(this).parent()

        thumbs.removeClass("upvoted")
        thumbs.toggleClass("downvoted")

        name = $(this).closest(".playerrow").data("name")

        if thumbs.hasClass("downvoted")
            downvote name
        else
            novote name
    
    $(".playerrow").on "click", ".playername", ->
        $.ajax { url: "/preview/" + $(this).parent().parent().data("name")
               , success: (x) -> replacePreview x  }


$(document).ready ->
    attachRow()

    $("form.addplayer").on "submit", (event) ->
        event.preventDefault()
        name = $(this).children("input").val()
        $.post "/newplayer/" + name, null, reloadTable
