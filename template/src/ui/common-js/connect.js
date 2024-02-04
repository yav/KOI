
// Assumes the following to be in scope:
//  uiRedraw:       draw state from scratch
//  uiSetQuestion:  set an eplanation for what we are asking
//  uiQuestion:     present a potential answer to a question
//  uiUpdate        present an update
const srvConnect = () => {
  const obj = new URL(window.location)
  const info = obj.searchParams
  const url = 'ws://' + obj.host + '/ws'
  console.log("Connecting to: " + url)
  const ws = new WebSocket(url)

  const playerId = info.get('player')
  const sendJSON = (obj) => ws.send(JSON.stringify(obj))

  ws.onopen = (e) => {
    console.log('Connected.')
    console.log('We are player: ' + playerId)
    ws.send(playerId)
    sendJSON({ tag: 'reload' })
  }

  ws.onmessage = (e) => {
    const msg = JSON.parse(e.data)
    console.log('Received:')
    console.log(msg)
    switch (msg.tag) {
      case "CurGameState":  return uiRedraw(msg.contents)
      case "SetQuestion":   return uiSetQuestion(msg.contents)
      case "AskQuestions":  return uiQuestions(msg.contents)
      case "GameUpdate":    return uiUpdate(msg.contents)
    }
  }

  ws.onclose = (e) => {
    console.log('Disconnected.')
  }

  ws.onerror = (e) => {
    console.log('error')
    console.log(e)
  }

  const conn = {}
  conn.playerId = playerId
  conn.sendJSON = sendJSON
  return conn
}

// Ask multiple questions.  This in not inlined above, because the state
// conatins questions, and we can ask them using this when drawing it.
function uiQuestions(qs0) {
  const q = qs0[0]
  uiSetQuestion(q)
  const qs = qs0[1]
  for (let i = 0; i < qs.length; ++i) {
    uiQuestion(qs[i])
  }
}


