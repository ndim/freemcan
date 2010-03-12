<?xml version="1.0" encoding="utf-8"?>
<xsl:transform
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0">

  <xsl:output method="xml"
              version="1.0"
              encoding="us-ascii"
	      indent="yes"/>

  <xsl:template match="*">
    <xsl:element name="{name()}" namespace="{namespace-uri()}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="text()|comment()|*"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="property[text()='@LICENSE@']">
    <xsl:element name="{name()}" namespace="{namespace-uri()}">
      <xsl:apply-templates select="@*"/>
      <xsl:value-of select="document('lgpl-2.1.xml')/text/text()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="@*|text()|comment()">
    <xsl:copy/>
  </xsl:template>

</xsl:transform>
