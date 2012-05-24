state = { pageNum : %{show pageNum}
        , origPage : %{show pageNum}
        , currentSearch : null
        }

partial = (func, a...) ->
  (b...) -> func a..., b...

@unhide = (preview) ->
    item = document.getElementById(preview)
    if item
        item.className = if (item.className == 'hidden') then 'unhidden' else 'hidden'

reloadTable = ->
    searchTerm = state.currentSearch
    params = if searchTerm == null
                 {page: state.pageNum}
             else
                 {search: searchTerm}
    $.ajax { url: "/table"
           , data: params
           , success: (x) -> $("#players").replaceWith x; attachRow() }

vote = (dir, name, callback) -> $.post "/vote", JSON.stringify({name:name,vote:dir}), reloadTable
downvote = partial vote, "down"
upvote   = partial vote, "up"
novote   = partial vote, "neutral"

attachSearch = ->
    $("#searchinput").on "keyup", ->
        state.pageNum = 1
        state.currentSearch = $(this).val()
        reloadTable()

loadPreview = (name, callback) ->
        $.ajax { url: "/preview"
               , data: JSON.stringify([name])
               , type: "GET"
               , success: callback  }

replacePreview = (name) ->
    action = (x) ->
        $("#preview").replaceWith x
        $("#preview").show()
        $("#previewclose").click(-> $("#preview").hide())
        $("#previewPost").on "click submit", "#ppostbutton", (event) ->
            event.preventDefault()
            post = { player: $("#previewPost").data("player")
                   , name: $("#prevPostName").val()
                   , text: $("#ptext").val()
                   }
            if $("#prevPostUrl").val() != ""
                post.url = $("#prevPostUrl").val()
            $.post "/newpost", JSON.stringify(post), -> replacePreview name
    loadPreview(name, action)


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
        name = $(this).parent().parent().data("name")
        replacePreview name


attachAdd = ->
    $("#addplayerbottom").hide()
    $(".addbutton").on "click", ->
        $("#addplayerbottom").show()
        $("#addplayerbottom").children("input").focus()
    $(".addplayerpopupbutton").on "click", ->
        $("#addplayerbottom").hide()
    $("form.addplayer").on "submit", (event) ->
        event.preventDefault()
        name = $(this).children("input").val()
        if (name.length >= 2 && name.length <= 16)
            jsonName = JSON.stringify [name]
            $.post "/newplayer", jsonName, reloadTable
            $("#addplayerbottom").fadeOut()

    $("form.addplayer").on "focusout", ->
            $("#addplayerbottom").fadeOut()
        

$(document).ready ->
    attachRow()
    attachAdd()
    attachSearch()
