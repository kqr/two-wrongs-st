<apply template="base">
    <h1><pageTitle /></h1>

    <h2><datestamp /></h2>

    <isTagged>
        <p>Tagged with:</p>

        <ul>
            <tags>
                <li><a href="${tagURL}"><tagName /></a></li>
            </tags>
        </ul>
    </isTagged>

    <content />
</apply>
