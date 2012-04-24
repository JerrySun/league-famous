
$(document).ready ->
    $(".uparrow").each ->
        name = $(this).data("name")
        $(this).click ->
            $(this).toggleClass("activeup")
            $.get "/upvote/" + name, null, -> 
                $.ajax { url: "/table"
                       , success: (x) -> $("#votes").replaceWith x }


    $(".downarrow").each ->
        name = $(this).data("name")
        $(this).click ->
            $(this).toggleClass("activedown")
            $.get "/downvote/" + name, null, -> 
                $.ajax { url: "/table"
                       , success: (x) -> $("#votes").replaceWith x }
