export default
function newValue() {
  let value = ""
  const dom = document.createElement("div")

  return {
    dom: dom,
    getValue: () => value,
    setValue: (x: string) => {
      if (value == x) return
      dom.textContent = x
    }
  }
}
