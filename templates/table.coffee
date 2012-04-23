
$(document).ready ->
    $(".uparrow").each ->
        name = $(this).data("name")
        $(this).click ->
            $.get "/upvote/" + name
            $(this).toggleClass("activeup")
            $.ajax { url: "/table"
                   , success: (x) -> $("#votes").replaceWith x }


    $(".downarrow").each ->
        name = $(this).data("name")
        $(this).click ->
            $.get "/downvote/" + name
            $(this).toggleClass("activedown")
            $.ajax { url: "/table"
                   , success: (x) -> $("#votes").replaceWith x }
