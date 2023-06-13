$(document).on('ready turbolinks:load', () => {

    // Init sortable.
    document.querySelectorAll('.js-sortable').forEach(function (elem) {
        if (Boolean(elem.jsSortableInitialized) === false) {
            Sortable.create(elem, {
                handle: '.sortable-handle',
                animation: 150,
            });
            elem.jsSortableInitialized = true;
        }
    });


    setupWysiwyg();
});

function setupWysiwyg() {
    const textAreaF = document.querySelector('[control-type=wysiwyg]');

    if (!textAreaF)
    {
        return;
    } 

    textAreaF.style.display = 'none';
    const p = document.createElement("div");
    p.id = "editor";
    textAreaF.after(p);

    quill = new Quill('#editor', {
        theme: 'snow'
    });

    quill.root.innerHTML = textAreaF.value;
    quill.on('text-change', () => textAreaF.value = quill.root.innerHTML);
}

