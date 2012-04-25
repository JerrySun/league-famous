unhide = (preview) -> if (item)
    item.className = if (item.className == 'hidden') then 'unhidden' else 'hidden'
