import {
  defineComponent,
  ref,
  reactive,
  computed,
} from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';
import { ShipSelector } from '../ship-selector.mjs';
import { Battlefield } from '../battlefield.mjs';
import { ships as allShips } from '../ships.mjs';
import { submitPlacements } from '../api.mjs';
import { BATTLEFIELD_ROWS, BATTLEFIELD_COLS } from '../../config.mjs';

export const SetupView = defineComponent({
  name: 'Setup View',
  template: '#setup-view',
  props: {
    game: Object,
  },
  components: { ShipSelector, Battlefield },
  setup(props) {
    const isSetupDone = computed(() => {
      return (
        (props.game.state.contents[0] === true && props.game.pType === 'Owner') ||
        (props.game.state.contents[1] === true && props.game.pType === 'Guest')
      );
    });
    const ships = ref(
      allShips.map((ship) => ({
        disabled: false,
        direction: 'RightWards',
        ...ship,
      })),
    );
    const selectedShip = ref(undefined);
    const placedShips = reactive([]);

    const onChangeDirection = () => {
      selectedShip.value.direction = selectedShip.value.direction === 'RightWards' ? 'DownWards' : 'RightWards';
    };

    const onShipPlaced = ({ startPoint, endPoint }) => {
      placedShips.push({
        startPoint: { row: startPoint.row, col: startPoint.col },
        endPoint: { row: endPoint.row, col: endPoint.col },
        ...selectedShip.value,
      });
      selectedShip.value.disabled = true;
      selectedShip.value = undefined;
    };

    const reset = () => {
      placedShips.length = 0;
      selectedShip.value = undefined;
      ships.value = ships.value.map((ship) => {
        ship.disabled = false;
        return ship;
      });
    };

    const canSubmit = computed(() => placedShips.length === allShips.length);
    const submit = async () => {
      if (!canSubmit.value) return;
      const placements = placedShips.map((ship) => [ship.id, ship.startPoint.row, ship.startPoint.col, ship.direction]);
      const response = submitPlacements(placements);
    };

    return {
      isSetupDone,
      ships,
      reset,
      submit,
      canSubmit,
      selectedShip,
      onChangeDirection,
      battlefieldRows: BATTLEFIELD_ROWS,
      battlefieldCols: BATTLEFIELD_COLS,
      onShipPlaced,
      placedShips,
    };
  },
});
