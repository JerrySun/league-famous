partial = (func, a...) ->
  (b...) -> func a..., b...

@unhide = (preview) ->
    item = document.getElementById(preview)
    if item
        item.className = if (item.className == 'hidden') then 'unhidden' else 'hidden'

reloadTable = -> $.ajax { url: "/table"
                        , success: (x) -> $("#players").replaceWith x; attachRow() }

vote = (dir, name, callback) -> $.post "/vote", JSON.stringify({name:name,vote:dir}), reloadTable
downvote = partial vote, "down"
upvote   = partial vote, "up"
novote   = partial vote, "neutral"


replacePreview = (x) ->
    $("#preview").replaceWith x
    $("#preview").show()
    $("#previewclose").click(-> $("#preview").hide())

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
        $.ajax { url: "/preview"
               , data: JSON.stringify([$(this).parent().parent().data("name")])
               , type: "GET"
               , success: (x) -> replacePreview x  }


attachAdd = ->
    $("#addplayerbottom").hide()
    $(".addbutton").on "click", ->
        $("#addplayerbottom").show()
    $(".addplayerpopupbutton").on "click", ->
        $("#addplayerbottom").hide()
    $("form.addplayer").on "submit", (event) ->
        event.preventDefault()
        name = $(this).children("input").val()
        jsonName = JSON.stringify [name]
        $.post "/newplayer", jsonName, reloadTable
        $("#addplayerbottom").fadeOut()

$(document).ready ->
    attachRow()
    attachAdd()
