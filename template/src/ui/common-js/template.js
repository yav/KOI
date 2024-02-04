
function uiFromTemplate(id) {
  const orig = document.getElementById(id)
  if (!orig) { console.log("Failed to find template", id) }
  const dom = orig.cloneNode(true)
  dom.removeAttribute("id")
  return dom
}

function uiFromTemplateNested(id) {
  const dom = uiFromTemplate(id)
  const els = {}
  function search(it) {
    for (const el of it.children) {
      const a = el.getAttribute("id")
      if (a !== undefined) {
        els[a] = el
        el.removeAttribute("id")
      }
      search(el)
    }
  }
  search(dom)
  return [ dom, els ]
}

