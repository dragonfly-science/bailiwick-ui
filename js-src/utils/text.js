import { present } from './utils'

var textSubstitution = function(s, bw, addCompareArea) {
    var y = bw.get('year.name'),
        fy = bw.get('indicator.firstYear.name'),
        yem = bw.get('indicator.yearEndMonth'),
        su = bw.get('indicator.subject'),
        a = bw.get('area.name'),
        sa = bw.get('area.name'),
        f = bw.get('feature.name'),
        fl = f || bw.get('indicator.topFeatureLabel'),
        fp = f ? bw.get('feature.parent') : "",
        d = bw.get('detail.name'),
        dl = d || bw.get('indicator.topDetailLabel'),
        ft = bw.get('indicator.featureText'),
        p = bw.get("prevYear"),
        ca = bw.get("compareArea.name");
    if (present(addCompareArea) && present(ca)) {
        a = "<span class='active'>" + a + "</span><span class='compare'> (and " + ca + ")</span>";
    }
    if (ft) {
        var fs = bw.get('feature.id');
        fl = ft[fs];
    }
    if (f) {
        s = s.replace(/[\[\]]/g, '');
    } else {
        s = s.replace(/\[.+?\]/g, '');
    }
    if (d) {
        s = s.replace(/[\{\}]/g, '');
    } else {
        s = s.replace(/\s?\{.+?\}/g, '');
    }
    s = s.replace(/\$year\$/g, y)
        .replace(/\$firstYear\$/g, fy)
        .replace(/\$yearEndMonth\$/g, yem)
        .replace(/\$subject\$/g, su)
        .replace(/\$area\$/g, a)
        .replace(/\$selectedArea\$/g, sa)
        .replace(/\$compareArea\$/g, ca)
        .replace(/\$prevYear\$/g, p)
        .replace(/\$feature\$/g, fl)
        .replace(/\$featureType\$/g, fp)
        .replace(/\$detail\$/g, dl);
    return s.trim();
}

var label = function(transform) {
    let transforms = ["absolute", "indexed", "percapita"];
    
    if (transforms.indexOf(transform) === -1) {
        if (transform === "absolute") {
            transform = "original";
        } else if (transform === "regionalPercentage") {
            transform = "feature-percentage";
        } else if (transform === "rate") {
            transform = "annual-rate";
        } else {
            return "";
        }
    }

    var l = textSubstitution(this.get("indicator.labels")[transform], this);
    return l[0].toUpperCase() + l.slice(1);
};

export { textSubstitution, label }