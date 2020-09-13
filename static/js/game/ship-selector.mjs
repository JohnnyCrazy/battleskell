import { defineComponent, computed } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';

export const ShipSelector = defineComponent({
  name: 'Ship Selector',
  template: '#ship-selector',
  emits: ['change-direction', 'update:modelValue'],
  props: {
    ships: Array,
    modelValue: Object,
  },
  setup(props, { emit }) {
    const selectedShip = computed({
      get: () => props.modelValue,
      set: (val) => emit('update:modelValue', val),
    });

    const changeDirection = () => {
      emit('change-direction');
    };

    return {
      ships: props.ships,
      selectedShip,
      changeDirection,
    };
  },
});
