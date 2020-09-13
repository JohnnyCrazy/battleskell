import { defineComponent, computed } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';

export const BattlefieldCell = defineComponent({
  name: 'Battlefield Cell',
  template: '#battlefield-cell',
  props: {
    cell: Object,
  },
  emits: ['hover'],
  setup(props, { emit }) {
    const onHover = () => {
      emit('hover');
    };

    const classes = computed(() => {
      if (props.cell.state === 'empty') {
        return 'bg-black';
      } else if (props.cell.state === 'placed') {
        return props.cell.color ? `bg-${props.cell.color}` : 'bg-gray-500';
      } else if (props.cell.state === 'invalid') {
        return 'bg-red-700';
      } else if (props.cell.state === 'place-possible') {
        return 'bg-green-500';
      }
    });

    const crossClasses = computed(() => {
      return props.cell.hit ? 'text-red-500' : 'text-white';
    });

    const isHit = computed(() => props.cell.hit);

    return {
      onHover,
      isHit,
      classes,
      crossClasses,
    };
  },
});
