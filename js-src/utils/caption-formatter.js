import { none, present } from './utils'

export default function(indicatorUnits, transform, caption) {
    // var _this = this;
    // var indicator = this.get("indicator");
    // var currentTransform = this.get("chartInfo").transforms2.find(function(o) {
    //   return o.name === _this.get("dataTransform");
    // });
    // if (this.get("model") === "map") {
    //   currentTransform = this.get("chartInfo").transforms2[0];
    // }

    // var text = "  ";
    // var fstr = "";
    // if (present(transform) && present(caption)) {
    // //   text = textSubstitution(s, params, addCompareArea);
    //   fstr = currentTransform.formatter;
    //   if (none(fstr)) {
    //     switch (currentTransform.name) {
    //       case "indexed":
    //         fstr = "indexed";
    //         break;
    //       case "regionalPercentage":
    //       case "nationalPercentage":
    //       case "rate":
    //         fstr = "percentage";
    //         break;
    //       case "absolute":
    //         fstr = indicator.get("model.units");
    //         break;
    //       default:
    //         fstr = "count";
    //     }
    //   }

    // }

    // return Ember.Object.create({
    //   text: text ? text[0].toUpperCase() + text.slice(1) : "",
    //   formatter: function(d) {
    //     return formatting(fstr, d);
    //   }
    // });
}