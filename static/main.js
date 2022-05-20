import pev2 from 'https://cdn.jsdelivr.net/npm/pev2@0.24.0/dist/components/pev2.umd.js';

var app = document.getElementById("app");
app.innerHTML = `<pev2 :plan-source="plan" :plan-query="query"></pev2>`;
new Vue({
    el: "#app",
    data: function(){
        return {
            plan: planSource,
            query: planQuery
        };
    },
    components: {
        pev2
    }
});//.$mount("#app");
