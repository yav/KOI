
export
function uiFromTemplate(id: string) : HTMLElement {
  const orig = document.getElementById(id)
  if (!orig) { throw new Error("Missing template: " + id) }
  const dom = orig.cloneNode(true) as HTMLElement
  dom.removeAttribute("id")
  return dom
}

export
function uiFromTemplateNested(id: string)
  : [ HTMLElement, { [domId:string]: HTMLElement } ] {
  const dom = uiFromTemplate(id)
  const els = {}
  function search(it: HTMLElement) {
    for (const el of it.children) {
      const a = el.getAttribute("id")
      if (a !== null) {
        els[a] = el
        el.removeAttribute("id")
      }
      if (el instanceof HTMLElement) { search(el) }
    }
  }
  search(dom)
  return [ dom, els ]
}

