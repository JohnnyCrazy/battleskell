import { defineComponent, reactive, computed } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';
import { isBetween, allPointsBetween } from '../util.mjs';
import { BattlefieldCell } from './battlefield-cell.mjs';

const useShipWithPositions = (props, hoveredCell) =>
  computed(() => {
    if (!props.shipToPlace || !hoveredCell) return undefined;

    const startPoint = hoveredCell;
    const endPoint =
      props.shipToPlace.direction === 'RightWards'
        ? { row: hoveredCell.row, col: hoveredCell.col + props.shipToPlace.length - 1 }
        : { row: hoveredCell.row + props.shipToPlace.length - 1, col: hoveredCell.col };
    const allPoints = allPointsBetween(startPoint, endPoint);

    const ship = props.ships.find((ship) => allPoints.some((p) => isBetween(ship.startPoint, ship.endPoint, p)));

    return {
      startPoint,
      endPoint,
      isValid:
        !ship &&
        isBetween({ row: 0, col: 0 }, { row: props.rows - 1, col: props.cols - 1 }, startPoint) &&
        isBetween({ row: 0, col: 0 }, { row: props.rows - 1, col: props.cols - 1 }, endPoint),
    };
  });

const useHoverState = () => {
  const hoveredCell = reactive({ row: -1, col: -1 });

  const onCellHover = (row, col) => {
    hoveredCell.row = row;
    hoveredCell.col = col;
  };

  const onMouseOut = () => {
    hoveredCell.row = -1;
    hoveredCell.col = -1;
  };

  return {
    hoveredCell,
    onCellHover,
    onMouseOut,
  };
};

const useCellStates = (props, shipWithPos, hoveredCell) => {
  return computed(() => {
    const initialCellStates = reactive(
      new Array(props.rows).fill(0).map(() =>
        new Array(props.cols).fill(0).map(() => ({
          state: 'empty',
        })),
      ),
    );

    return initialCellStates.map((_, row) => {
      return initialCellStates[row].map((_, col) => {
        const cell = initialCellStates[row][col];

        if (shipWithPos.value) {
          const { startPoint, endPoint, isValid } = shipWithPos.value;
          if (isBetween(startPoint, endPoint, { row, col })) {
            cell.state = isValid ? 'place-possible' : 'invalid';
            cell.color = isValid ? props.shipToPlace.color : undefined;
            return cell;
          }
        }

        const ship = props.ships.find((ship) => isBetween(ship.startPoint, ship.endPoint, { row, col }));
        cell.state = ship ? 'placed' : 'empty';
        cell.color = ship ? ship.color : undefined;

        const hit = props.hits.find(({ row: hitRow, col: hitCol }) => row == hitRow && col == hitCol);
        cell.hit = !!hit;
        cell.state = hit && hit.hitShip ? 'placed' : cell.state;

        if (props.canFire && hoveredCell.row === row && hoveredCell.col === col) {
          cell.state = cell.hit ? 'invalid' : 'place-possible';
        }

        return cell;
      });
    });
  });
};

export const Battlefield = defineComponent({
  name: 'Battlefield',
  template: '#battlefield',
  props: {
    rows: Number,
    cols: Number,
    ships: Array,
    shipToPlace: Object,
    canFire: {
      type: Boolean,
      default: false,
    },
    hits: {
      type: Array,
      default: [],
    },
  },
  emits: ['hover', 'ship-placed', 'fire'],
  components: { BattlefieldCell },
  setup(props, { emit }) {
    const rowNames = new Array(props.rows).fill(0).map((_, i) => String.fromCharCode('A'.charCodeAt(0) + i));
    const colNames = new Array(props.cols).fill(0).map((_, i) => i + 1);

    const { onCellHover, onMouseOut, hoveredCell } = useHoverState();
    const shipWithPos = useShipWithPositions(props, hoveredCell);
    const cellStates = useCellStates(props, shipWithPos, hoveredCell);

    const onClick = (row, col) => {
      if (props.canFire && hoveredCell.row !== -1 && hoveredCell.col !== -1) {
        emit('fire', hoveredCell);
      }
      if (shipWithPos.value && shipWithPos.value.isValid) {
        emit('ship-placed', shipWithPos.value);
      }
    };

    return {
      colNames,
      rowNames,
      onCellHover,
      onClick,
      cellStates,
      shipWithPos,
      onMouseOut,
    };
  },
});
