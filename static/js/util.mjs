import { onUnmounted } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';

export function createPingSocket(url, onMessage) {
  let pingTimer = undefined;

  const protocol = location.protocol === 'http:' ? 'ws:' : 'wss:';

  const socket = new WebSocket(`${protocol}//${url}`);
  socket.onopen = (e) => {
    pingTimer = setInterval(() => socket.send(JSON.stringify('ping')), 20000);
  };

  socket.onmessage = ({ data }) => {
    const { id, val } = JSON.parse(data);
    onMessage(id, val);
  };

  onUnmounted(() => {
    socket.close();
    clearInterval(pingTimer);
  });
}

export function isBetween({ row: startRow, col: startCol }, { row: endRow, col: endCol }, { row, col }) {
  return row >= startRow && row <= endRow && col >= startCol && col <= endCol;
}

export function allPointsBetween({ row: startRow, col: startCol }, { row: endRow, col: endCol }) {
  const points = [];
  for (let row = startRow; row <= endRow; row++) {
    for (let col = startCol; col <= endCol; col++) {
      points.push({ row, col });
    }
  }
  return points;
}
