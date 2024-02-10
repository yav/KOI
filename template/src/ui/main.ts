import { srvConnect, Connection, Question } from "./common-js/connect.ts"
import { uiFromTemplate } from "./common-js/template.ts"

type Q = "Inc" | "Dec"
type GUI = {
  counter: HTMLElement,
  question: HTMLElement,
  questions: HTMLElement[]
}

let conn: Connection<Q>;
let gui: GUI;

export default
function main () {
  const cb = {
    uiRedraw: uiRedraw,
    uiSetQuestion: uiSetQuestion,
    uiQuestion: uiQuestion,
    uiUpdate: uiUpdate
  }
  conn = srvConnect(cb)
}

// Redraw the whole state
function uiRedraw (state) {

  gui = {
    counter: uiFromTemplate("template-counter"),
    question: uiFromTemplate("template-question"),
    questions: []
  }

  const body = document.getElementById("content")
  if (body == null) { throw("Failed to find the body") }
  
  body.innerHTML = ""
  gui.counter.textContent = "?"
  body.appendChild(gui.counter)
  body.appendChild(gui.question)
  uiUpdate(state.game)
  conn.uiQuestions(state.questions)
}


// Set the explanation for what we are asking.
function uiSetQuestion (q: string) {
  gui.question.textContent = q
  gui.questions = []
}

// Various things that can be used to answer the question.
function uiQuestion (q: Question<Q>) {

  function btn(lab: string) {
    const dom = uiFromTemplate("template-btn")
    dom.textContent = lab
    dom.addEventListener("click", () => {
      const n = gui.questions.length
      for (let i = 0; i < n; ++i) gui.questions[i].remove()
      gui.question.textContent = ""
      conn.sendJSON(q)
    })

    const body = document.getElementById("content")
    if (body === null) { throw("Failed to find `content`") }
    body.appendChild(dom)
    gui.questions.push(dom)
  }

  switch(q.chChoice) {
    case "Inc": return btn("inc")
    case "Dec": return btn("dec")
  }
}


function uiUpdate(newS: string) {
  gui.counter.textContent = newS
}
