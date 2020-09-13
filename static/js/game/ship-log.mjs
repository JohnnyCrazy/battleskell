import { defineComponent, reactive, computed } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';

const leadingZero = (num) => `0${num}`.slice(-2);

const formatTime = (date) => [date.getHours(), date.getMinutes(), date.getSeconds()].map(leadingZero).join(':');

export const ShipLog = defineComponent({
  name: 'Ship Log',
  template: '#ship-log',
  props: {
    logs: {
      type: Array,
      default: [],
    },
    pType: {
      type: String,
    },
  },
  setup(props) {
    const logs = computed(() =>
      props.logs
        .map((log) => {
          const date = new Date(log.createdAt);
          return {
            ...log,
            from: log.playerType ? (props.pType === log.playerType ? 'YOU   ' : 'ENEMY ') : 'SYSTEM',
            createdAtStr: formatTime(date),
          };
        })
        .reverse(),
    );

    return {
      logs,
    };
  },
});
