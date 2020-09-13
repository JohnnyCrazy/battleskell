import {
  defineComponent,
  ref,
  reactive,
  computed,
} from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';
import { ships as allShips } from '../ships.mjs';

import { Battlefield } from '../battlefield.mjs';
import { ShipLog } from '../ship-log.mjs';
import { hitCell } from '../api.mjs';

const convertShips = (ships) => {
  return ships.map(([[id, row, col, dir], _]) => {
    const ship = allShips[id];
    const endPoint = dir === 'RightWards' ? { row, col: col + ship.length - 1 } : { row: row + ship.length - 1, col };
    return {
      startPoint: { row, col },
      endPoint,
      ...ship,
    };
  });
};

export const TurnView = defineComponent({
  name: 'Turn View',
  template: '#turn-view',
  components: { Battlefield, ShipLog },
  props: {
    game: Object,
  },
  setup(props) {
    const isMyTurn = computed(
      () =>
        (props.game.pType === 'Guest' && props.game.state.tag == 'GuestTurn') ||
        (props.game.pType === 'Owner' && props.game.state.tag == 'OwnerTurn'),
    );

    const myShips = computed(() => convertShips(props.game.stateView.you.ships));
    const enemyShips = computed(() => convertShips(props.game.stateView.enemy.ships));

    const myHittedCells = computed(() => {
      return props.game.stateView.you.hits.map(([row, col, hitShip]) => ({ row, col, hitShip }));
    });
    const enemyHittedCells = computed(() => {
      return props.game.stateView.enemy.hits.map(([row, col, hitShip]) => ({ row, col, hitShip }));
    });

    const onFire = async (pos) => {
      await hitCell(pos);
    };

    return {
      isMyTurn,
      myShips,
      myHittedCells,
      enemyShips,
      enemyHittedCells,
      onFire,
    };
  },
});
