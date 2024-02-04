let sendJSON = null
let playerId = null
let gui      = null

function main () {
  const conn = srvConnect()
  playerId = conn.playerId
  sendJSON = conn.sendJSON
}

// Redraw the whole state
function uiRedraw (state) {
  const body = document.getElementById("content")
  body.innerHTML = ""
  gui = {}

  gui.counter = uiFromTemplate("template-counter")
  gui.counter.textContent = "?"
  body.appendChild(gui.counter)

  gui.question = uiFromTemplate("template-question")
  body.appendChild(gui.question)

  uiUpdate(state.game)
  uiQuestions(state.questions)
}


// Set the explanation for what we are asking.
function uiSetQuestion (q) {
  gui.question.textContent = q
  gui.questions = []
}

// Various things that can be used to answer the question.
function uiQuestion (q) {
  const body = document.getElementById("content")

  function btn(lab) {
    const dom = uiFromTemplate("template-btn")
    dom.textContent = lab
    dom.addEventListener("click", () => {
      const n = gui.questions.length
      for (let i = 0; i < n; ++i) gui.questions[i].remove()
      gui.question.textContent = ""
      sendJSON(q)
    })
    body.appendChild(dom)
    gui.questions.push(dom)
  }

  switch(q.chChoice) {
    case "Inc": return btn("inc")
    case "Dec": return btn("dec")
  }

  console.log("OTHER", q)
}


function uiUpdate(newS) {
  gui.counter.textContent = newS
}


