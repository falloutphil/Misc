<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns="http://www.w3.org/1999/xhtml">

<xsl:output method="html" indent="yes" encoding="iso-8859-1" />

<xsl:param name="location"/>
<xsl:param name="rowstyle"/>

<xsl:template match="node()|@*">

<xsl:apply-templates select="node()|@*"/>
</xsl:template>

<xsl:template match="/response/trip">
<tr class="{$rowstyle}">
<td><xsl:value-of select="$location"/></td>
<td><xsl:value-of select="temp_high/avg/C"/><xsl:text>&#176;C</xsl:text></td>
<td><xsl:value-of select="temp_low/avg/C"/><xsl:text>&#176;C</xsl:text></td>
<td><xsl:value-of select="precip/avg/cm"/><xsl:text>cm</xsl:text></td>
<td><xsl:value-of select="chance_of/chanceofsunnycloudyday/percentage"/><xsl:text>%</xsl:text></td>
<td><xsl:value-of select="chance_of/chanceofpartlycloudyday/percentage"/><xsl:text>%</xsl:text></td>
<td><xsl:value-of select="chance_of/chanceofcloudyday/percentage"/><xsl:text>%</xsl:text></td>
<td><xsl:value-of select="chance_of/chanceofrainday/percentage"/><xsl:text>%</xsl:text></td>
<td><xsl:value-of select="chance_of/chanceofwindyday/percentage"/><xsl:text>%</xsl:text></td>
</tr>
</xsl:template>

</xsl:stylesheet>

