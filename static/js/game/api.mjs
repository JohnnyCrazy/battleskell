export function submitPlacements(placements) {
  return fetch(location.href + '/state', {
    method: 'POST',
    body: JSON.stringify({ id: 'setup', val: placements }),
    headers: { accept: 'application/json', 'content-type': 'application/json' },
  });
}

export function hitCell(position) {
  return fetch(location.href + '/state', {
    method: 'POST',
    body: JSON.stringify({ id: 'hit', val: position }),
    headers: { accept: 'application/json', 'content-type': 'application/json' },
  });
}
