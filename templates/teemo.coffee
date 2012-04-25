@unhide = (preview) ->
    item = document.getElementById(preview)
    if item
        item.className = if (item.className == 'hidden') then 'unhidden' else 'hidden'
